/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package interactive

import scala.collection._
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.util.control.Breaks._
import scala.tools.nsc.symtab.Flags

import dependencies._
import util.FakePos
import io.AbstractFile

/** A more defined build manager, based on change sets. For each
 *  updated source file, it computes the set of changes to its
 *  definitions, then checks all dependent units to see if the
 *  changes require a compilation. It repeats this process until
 *  a fixpoint is reached.
 */
class RefinedBuildManager(val settings: Settings) extends Changes with BuildManager {

  class BuilderGlobal(settings: Settings, reporter : Reporter) extends scala.tools.nsc.Global(settings, reporter)  {

    def this(settings: Settings) =
      this(settings, new ConsoleReporter(settings))

    override def computeInternalPhases() {
      super.computeInternalPhases
      phasesSet += dependencyAnalysis
    }

    def newRun() = new Run()
  }

  protected def newCompiler(settings: Settings) = new BuilderGlobal(settings)

  val compiler = newCompiler(settings)
  import compiler.{Symbol, Type, atPhase, currentRun}

  private case class SymWithHistory(sym: Symbol, befErasure: Type)

  /** Managed source files. */
  private val sources: mutable.Set[AbstractFile] = new mutable.HashSet[AbstractFile]

  private val definitions: mutable.Map[AbstractFile, List[SymWithHistory]] =
    new mutable.HashMap[AbstractFile, List[SymWithHistory]] {
      override def default(key: AbstractFile) = Nil
    }

  /** External references used by source file. */
  private var references: mutable.Map[AbstractFile, immutable.Set[String]] = _

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
      changes += sym -> List(Removed(Class(sym.fullNameString)))
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

    // See if we really have coresponding symbols, not just those
    // which share the name
    def isCorrespondingSym(from: Symbol, to: Symbol): Boolean =
      (from.hasFlag(Flags.TRAIT) == to.hasFlag(Flags.TRAIT)) &&
      (from.hasFlag(Flags.MODULE) == to.hasFlag(Flags.MODULE))

    def update0(files: Set[AbstractFile]): Unit = if (!files.isEmpty) {
      deleteClassfiles(files)
      val run = compiler.newRun()
      compiler.inform("compiling " + files)
      buildingFiles(files)

      run.compileFiles(files.toList)
      if (compiler.reporter.hasErrors) {
        compiler.reporter.reset
        return
      }

      val changesOf = new mutable.HashMap[Symbol, List[Change]]
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
               s => (s.sym.fullNameString == sym.fullNameString) &&
                    isCorrespondingSym(s.sym, sym)) match {
              case Some(SymWithHistory(oldSym, info)) =>
                val changes = changeSet(oldSym.info, sym)
                val changesErasure =
                    atPhase(currentRun.erasurePhase.prev) {
                        changeSet(info, sym)
                    }
                changesOf(oldSym) = (changes ++ changesErasure).removeDuplicates
              case _ =>
                // a new top level definition
                changesOf(sym) =
                    sym.info.parents.filter(_.typeSymbol hasFlag Flags.SEALED).map(
                      p => changeChangeSet(p.typeSymbol,
                                           sym+" extends a sealed "+p.typeSymbol))
            }
          }
          // Create a change for the top level classes that were removed
          val removed = definitions(src) filterNot ((s:SymWithHistory) =>
            syms.find(_.fullNameString == (s.sym.fullNameString)) != None)
          for (s <- removed) {
            changesOf(s.sym) = List(removeChangeSet(s.sym))
          }
        }
      }
      println("Changes: " + changesOf)
      updateDefinitions(files)
      val invalid = invalidated(files, changesOf, additionalDefs)
      update0(checkCycles(invalid, files, coll))
    }

    update0(files)
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
      println("invalidate " + file + " because " + reason + " [" + change + "]")
      buf += file
      directDeps -= file
      for (syms <- definitions(file))     // fixes #2557
        newChangesOf(syms.sym) = List(change)
      break
    }

    for ((oldSym, changes) <- changesOf; change <- changes) {
      def checkParents(cls: Symbol, file: AbstractFile) {
        val parentChange = cls.info.parents.exists(_.typeSymbol.fullNameString == oldSym.fullNameString)
          // println("checkParents " + cls + " oldSym: " + oldSym + " parentChange: " + parentChange + " " + cls.info.parents)
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
            if (cls.info.decls.iterator.exists(_.fullNameString == name))
              invalidate(file, "of new method with existing name", change)
          case Changed(Class(name)) =>
            if (cls.info.typeSymbol.fullNameString == name)
              invalidate(file, "self type changed", change)
          case _ =>
            ()
        }
      }

      def checkReferences(file: AbstractFile) {
        // println(file + ":" + references(file))
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

        for (file <- directDeps) {
          breakable {
            for (cls <- definitions(file)) checkParents(cls.sym, file)
            for (cls <- definitions(file)) checkInterface(cls.sym, file)
            checkReferences(file)
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
    for (src <- files; val localDefs = compiler.dependencyAnalysis.definitions(src)) {
      definitions(src) = (localDefs map (s => {
        SymWithHistory(
          s.cloneSymbol,
          atPhase(currentRun.erasurePhase.prev) {
            s.info.cloneInfo(s)
          })
      }))
    }
    this.references = compiler.dependencyAnalysis.references
  }

  /** Load saved dependency information. */
  def loadFrom(file: AbstractFile, toFile: String => AbstractFile) : Boolean = {
    val success = compiler.dependencyAnalysis.loadFrom(file, toFile)
    if (success)
      sources ++= compiler.dependencyAnalysis.managedFiles
    success
  }

  /** Save dependency information to `file'. */
  def saveTo(file: AbstractFile, fromFile: AbstractFile => String) {
    compiler.dependencyAnalysis.dependenciesFile = file
    compiler.dependencyAnalysis.saveDependencies(fromFile)
  }
}
