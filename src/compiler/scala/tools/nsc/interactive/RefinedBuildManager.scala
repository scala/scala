/* NSC -- new Scala compiler
 * Copyright 2009-2011 Scala Solutions and LAMP/EPFL
 * @author Iulian Dragos
 * @author Hubert Plocinicak
 */
package scala.tools.nsc
package interactive

import scala.collection._
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.util.control.Breaks._
import scala.tools.nsc.symtab.Flags

import dependencies._
import scala.reflect.internal.util.FakePos
import util.ClassPath
import io.AbstractFile
import scala.tools.util.PathResolver

/** A more defined build manager, based on change sets. For each
 *  updated source file, it computes the set of changes to its
 *  definitions, then checks all dependent units to see if the
 *  changes require a compilation. It repeats this process until
 *  a fixpoint is reached.
 */
@deprecated("Use sbt incremental compilation mechanism", "2.10.0")
class RefinedBuildManager(val settings: Settings) extends Changes with BuildManager {

  class BuilderGlobal(settings: Settings, reporter : Reporter) extends scala.tools.nsc.Global(settings, reporter)  {

    def this(settings: Settings) =
      this(settings, new ConsoleReporter(settings))

    override def computeInternalPhases() {
      super.computeInternalPhases
      phasesSet += dependencyAnalysis
    }
    lazy val _classpath = new NoSourcePathPathResolver(settings).result
    override def classPath = _classpath.asInstanceOf[ClassPath[platform.BinaryRepr]]
       // See discussion in JavaPlatForm for why we need a cast here.

    def newRun() = new Run()
  }

  class NoSourcePathPathResolver(settings: Settings) extends PathResolver(settings) {
    override def containers = Calculated.basis.dropRight(1).flatten.distinct
  }

  protected def newCompiler(settings: Settings) = new BuilderGlobal(settings)

  val compiler = newCompiler(settings)
  import compiler.{ Symbol, Type, beforeErasure }
  import compiler.dependencyAnalysis.Inherited

  private case class SymWithHistory(sym: Symbol, befErasure: Type)

  /** Managed source files. */
  private val sources: mutable.Set[AbstractFile] = new mutable.HashSet[AbstractFile]

  private val definitions: mutable.Map[AbstractFile, List[SymWithHistory]] =
    new mutable.HashMap[AbstractFile, List[SymWithHistory]] {
      override def default(key: AbstractFile) = Nil
    }

  /** External references used by source file. */
  private var references: mutable.Map[AbstractFile, immutable.Set[String]] = _

  /** External references for inherited members */
  private var inherited: mutable.Map[AbstractFile, immutable.Set[Inherited]] = _

  /** Reverse of definitions, used for caching */
  private var classes: mutable.Map[String, AbstractFile] =
    new mutable.HashMap[String, AbstractFile] {
      override def default(key: String) = null
  }

  /** Add the given source files to the managed build process. */
  def addSourceFiles(files: Set[AbstractFile]) {
    sources ++= files
    update(files)
  }

  /** Remove the given files from the managed build process. */
  def removeFiles(files: Set[AbstractFile]) {
    sources --= files
    deleteClassfiles(files)
    update(invalidatedByRemove(files))
  }

  /** Return the set of invalidated files caused by removing the given files.
   */
  private def invalidatedByRemove(files: Set[AbstractFile]): Set[AbstractFile] = {
    val changes = new mutable.HashMap[Symbol, List[Change]]
    for (f <- files; SymWithHistory(sym, _) <- definitions(f))
      changes += sym -> List(Removed(Class(sym.fullName)))
    invalidated(files, changes)
  }

  def update(added: Set[AbstractFile], removed: Set[AbstractFile]) {
    sources --= removed
    deleteClassfiles(removed)
    update(added ++ invalidatedByRemove(removed))
  }

  /** The given files have been modified by the user. Recompile
   *  them and all files that depend on them. Only files that
   *  have been previously added as source files are recompiled.
   *  Files that were already compiled are taken out from the result
   *  of the dependency analysis.
   */
  private def update(files: Set[AbstractFile]) = {
    val coll: mutable.Map[AbstractFile, immutable.Set[AbstractFile]] =
        mutable.HashMap[AbstractFile, immutable.Set[AbstractFile]]()
    compiler.reporter.reset()

    // See if we really have corresponding symbols, not just those
    // which share the name
    def isCorrespondingSym(from: Symbol, to: Symbol): Boolean =
      (from.hasFlag(Flags.TRAIT) == to.hasFlag(Flags.TRAIT)) && // has to run in 2.8, so no hasTraitFlag
      (from.hasFlag(Flags.MODULE) == to.hasFlag(Flags.MODULE))

    // For testing purposes only, order irrelevant for compilation
    def toStringSet(set: Set[AbstractFile]): String =
      set.toList sortBy (_.name) mkString("Set(", ", ", ")")

    def update0(files: Set[AbstractFile]): Unit = if (!files.isEmpty) {
      deleteClassfiles(files)
      val run = compiler.newRun()
      if (settings.Ybuildmanagerdebug.value)
        compiler.inform("compiling " + toStringSet(files))
      buildingFiles(files)

      run.compileFiles(files.toList)
      if (compiler.reporter.hasErrors) {
        return
      }

      // Deterministic behaviour required by partest
      val changesOf = new mutable.HashMap[Symbol, List[Change]] {
          override def toString: String = {
            val changesOrdered =
              toList.map(e => {
                e._1.toString + " -> " +
                e._2.sortBy(_.toString).mkString("List(", ", ", ")")
              })
            changesOrdered.sorted.mkString("Map(", ", ", ")")
          }
      }
      val additionalDefs: mutable.HashSet[AbstractFile] = mutable.HashSet.empty

      val defs = compiler.dependencyAnalysis.definitions
      for (src <- files) {
        if (definitions(src).isEmpty)
          additionalDefs ++= compiler.dependencyAnalysis.
                             dependencies.dependentFiles(1, mutable.Set(src))
        else {
          val syms = defs(src)
          for (sym <- syms) {
            definitions(src).find(
               s => (s.sym.fullName == sym.fullName) &&
                    isCorrespondingSym(s.sym, sym)) match {
              case Some(SymWithHistory(oldSym, info)) =>
                val changes = changeSet(oldSym.info, sym)
                val changesErasure = beforeErasure(changeSet(info, sym))

                changesOf(oldSym) = (changes ++ changesErasure).distinct
              case _ =>
                // a new top level definition
                changesOf(sym) = sym.parentSymbols filter (_.isSealed) map (p =>
                    changeChangeSet(p, sym+" extends a sealed "+p))
            }
          }
          // Create a change for the top level classes that were removed
          val removed = definitions(src) filterNot ((s:SymWithHistory) =>
            syms.find(_.fullName == (s.sym.fullName)) != None)
          for (s <- removed) {
            changesOf(s.sym) = List(removeChangeSet(s.sym))
          }
        }
      }
      if (settings.Ybuildmanagerdebug.value)
        compiler.inform("Changes: " + changesOf)
      updateDefinitions(files)
      val invalid = invalidated(files, changesOf, additionalDefs)
      update0(checkCycles(invalid, files, coll))
    }

    update0(files)
    // remove the current run in order to save some memory
    compiler.dropRun()
  }

  // Attempt to break the cycling reference deps as soon as possible and reduce
  // the number of compilations to minimum without having too coarse grained rules
  private def checkCycles(files: Set[AbstractFile], initial: Set[AbstractFile],
                          collect: mutable.Map[AbstractFile, immutable.Set[AbstractFile]]):
    Set[AbstractFile] = {
      def followChain(set: Set[AbstractFile], rest: immutable.Set[AbstractFile]):
        immutable.Set[AbstractFile] = {
        val deps:Set[AbstractFile] = set.flatMap(
              s => collect.get(s) match {
                     case Some(x) => x
                     case _ => Set[AbstractFile]()
              })
          val newDeps = deps -- rest
          if (newDeps.isEmpty) rest else followChain(newDeps, rest ++ newDeps)
      }
      var res:Set[AbstractFile] = mutable.Set()
      files.foreach( f =>
        if (collect contains f) {
          val chain = followChain(Set(f), immutable.Set()) ++ files
          chain.foreach((fc: AbstractFile) => collect += fc -> chain)
          res ++= chain
        } else
          res += f
       )

      initial.foreach((f: AbstractFile) => collect += (f -> (collect.getOrElse(f, immutable.Set()) ++ res)))
      if (res.subsetOf(initial)) Set() else res
  }

  /** Return the set of source files that are invalidated by the given changes. */
  def invalidated(files: Set[AbstractFile], changesOf: collection.Map[Symbol, List[Change]],
                  processed: Set[AbstractFile] = Set.empty):
    Set[AbstractFile] = {
    val buf = new mutable.HashSet[AbstractFile]
    val newChangesOf = new mutable.HashMap[Symbol, List[Change]]
    var directDeps =
      compiler.dependencyAnalysis.dependencies.dependentFiles(1, files)

    def invalidate(file: AbstractFile, reason: String, change: Change) = {
      if (settings.Ybuildmanagerdebug.value)
        compiler.inform("invalidate " + file + " because " + reason + " [" + change + "]")
      buf += file
      directDeps -= file
      for (syms <- definitions(file))     // fixes #2557
        newChangesOf(syms.sym) = List(change, parentChangeSet(syms.sym))
      break
    }

    for ((oldSym, changes) <- changesOf; change <- changes) {
      def checkParents(cls: Symbol, file: AbstractFile) {
        val parentChange = cls.parentSymbols exists (_.fullName == oldSym.fullName)
          // if (settings.buildmanagerdebug.value)
          //   compiler.inform("checkParents " + cls + " oldSym: " + oldSym + " parentChange: " + parentChange + " " + cls.info.parents)
        change match {
          case Changed(Class(_)) if parentChange =>
            invalidate(file, "parents have changed", change)

          case Changed(Definition(_)) if parentChange =>
            invalidate(file, "inherited method changed", change)

          case Added(Definition(_)) if parentChange =>
            invalidate(file, "inherited new method", change)

          case Removed(Definition(_)) if parentChange =>
            invalidate(file, "inherited method removed", change)

          case _ => ()
        }
      }

      def checkInterface(cls: Symbol, file: AbstractFile) {
        change match {
          case Added(Definition(name)) =>
            if (cls.info.decls.iterator.exists(_.fullName == name))
              invalidate(file, "of new method with existing name", change)
          case Changed(Class(name)) =>
            if (cls.info.typeSymbol.fullName == name)
              invalidate(file, "self type changed", change)
          case _ =>
            ()
        }
      }

      def checkReferences(file: AbstractFile) {
        //if (settings.buildmanagerdebug.value)
        //  compiler.inform(file + ":" + references(file))
        val refs = references(file)
        if (refs.isEmpty)
          invalidate(file, "it is a direct dependency and we don't yet have finer-grained dependency information", change)
        else {
          change match {
            case Removed(Definition(name)) if refs(name) =>
              invalidate(file, "it references deleted definition", change)
            case Removed(Class(name)) if (refs(name)) =>
              invalidate(file, "it references deleted class", change)
            case Changed(Class(name)) if (refs(name)) =>
              invalidate(file, "it references changed class", change)
            case Changed(Definition(name)) if (refs(name)) =>
              invalidate(file, "it references changed definition", change)
            case Added(Definition(name)) if (refs(name)) =>
              invalidate(file, "it references added definition", change)
            case _ => ()
          }
        }
      }

      def checkInheritedReferences(file: AbstractFile) {
        val refs = inherited(file)
        if (!refs.isEmpty)
          change match {
            case ParentChanged(Class(name)) =>
              for (Inherited(q, member) <- refs.find(p => (p != null && p.qualifier == name));
                   classFile <- classes.get(q);
                   defs <- definitions.get(classFile);
                   s <- defs.find(p => p.sym.fullName == q)
                     if ((s.sym).tpe.nonPrivateMember(member) == compiler.NoSymbol))
                invalidate(file, "it references invalid (no longer inherited) definition", change)
              ()
            case _ => ()
        }
      }

        for (file <- directDeps) {
          breakable {
            for (cls <- definitions(file)) checkParents(cls.sym, file)
            for (cls <- definitions(file)) checkInterface(cls.sym, file)
            checkReferences(file)
            checkInheritedReferences(file)
          }
        }
    }
    if (buf.isEmpty)
      processed
    else
      invalidated(buf.clone() --= processed, newChangesOf, processed ++ buf)
  }

  /** Update the map of definitions per source file */
  private def updateDefinitions(files: Set[AbstractFile]) {
    for (src <- files; localDefs = compiler.dependencyAnalysis.definitions(src)) {
      definitions(src) = (localDefs map (s => {
        this.classes += s.fullName -> src
        SymWithHistory(s.cloneSymbol, beforeErasure(s.info.cloneInfo(s)))
      }))
    }
    this.references = compiler.dependencyAnalysis.references
    this.inherited = compiler.dependencyAnalysis.inherited
  }

  /** Load saved dependency information. */
  def loadFrom(file: AbstractFile, toFile: String => AbstractFile) : Boolean = {
    val success = compiler.dependencyAnalysis.loadFrom(file, toFile)
    if (success)
      sources ++= compiler.dependencyAnalysis.managedFiles
    success
  }

  /** Save dependency information to `file`. */
  def saveTo(file: AbstractFile, fromFile: AbstractFile => String) {
    compiler.dependencyAnalysis.dependenciesFile = file
    compiler.dependencyAnalysis.saveDependencies(fromFile)
  }
}
