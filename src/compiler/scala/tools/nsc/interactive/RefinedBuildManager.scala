package scala.tools.nsc
package interactive

import scala.collection._
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.util.control.Breaks._

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

  class BuilderGlobal(settings: Settings) extends scala.tools.nsc.Global(settings)  {

    override def computeInternalPhases() {
      super.computeInternalPhases
      phasesSet += dependencyAnalysis
    }

    def newRun() = new Run()
  }

  protected def newCompiler(settings: Settings) = new BuilderGlobal(settings)

  val compiler = newCompiler(settings)
  import compiler.Symbol

  /** Managed source files. */
  private val sources: mutable.Set[AbstractFile] = new mutable.HashSet[AbstractFile]

  private val definitions: mutable.Map[AbstractFile, List[Symbol]] =
    new mutable.HashMap[AbstractFile, List[Symbol]] {
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
    update(invalidatedByRemove(files))
  }

  /** Return the set of invalidated files caused by removing the given files.
   */
  private def invalidatedByRemove(files: Set[AbstractFile]): Set[AbstractFile] = {
    val changes = new mutable.HashMap[Symbol, List[Change]]
    for (f <- files; sym <- definitions(f))
      changes += sym -> List(Removed(Class(sym.fullNameString)))
    invalidated(files, changes)
  }

  def update(added: Set[AbstractFile], removed: Set[AbstractFile]) {
    sources --= removed
    update(added ++ invalidatedByRemove(removed))
  }

  /** The given files have been modified by the user. Recompile
   *  them and all files that depend on them. Only files that
   *  have been previously added as source files are recompiled.
   */
  private def update(files: Set[AbstractFile]): Unit = if (!files.isEmpty) {
    val run = compiler.newRun()
    compiler.inform("compiling " + files)
    buildingFiles(files)

    run.compileFiles(files.toList)
    if (compiler.reporter.hasErrors) {
      compiler.reporter.reset
      return
    }

    val changesOf = new mutable.HashMap[Symbol, List[Change]]

    val defs = compiler.dependencyAnalysis.definitions
    for (val src <- files; val syms = defs(src); val sym <- syms) {
      definitions(src).find(_.fullNameString == sym.fullNameString) match {
        case Some(oldSym) =>
          changesOf(oldSym) = changeSet(oldSym, sym)
        case _ =>
          // a new top level definition, no need to process
      }
    }
    println("Changes: " + changesOf)
    updateDefinitions(files)
    update(invalidated(files, changesOf))
  }

  /** Return the set of source files that are invalidated by the given changes. */
  def invalidated(files: Set[AbstractFile], changesOf: collection.Map[Symbol, List[Change]]): Set[AbstractFile] = {
    val buf = new mutable.HashSet[AbstractFile]
    var directDeps =
      compiler.dependencyAnalysis.dependencies.dependentFiles(1, files)

//    println("direct dependencies on " + files + " " + directDeps)
    def invalidate(file: AbstractFile, reason: String, change: Change) = {
      println("invalidate " + file + " because " + reason + " [" + change + "]")
      buf += file
      directDeps -= file
      break
    }

    // changesOf will be empty just after initialization with a saved
    // dependencies file.
    if (changesOf.isEmpty)
      buf ++= directDeps
    else {
      for ((oldSym, changes) <- changesOf; change <- changes) {

        def checkParents(cls: Symbol, file: AbstractFile) {
          val parentChange = cls.info.parents.exists(_.typeSymbol.fullNameString == oldSym.fullNameString)
//          println("checkParents " + cls + " oldSym: " + oldSym + " parentChange: " + parentChange + " " + cls.info.parents)
          change match {
            case Changed(Class(_)) if parentChange =>
              invalidate(file, "parents have changed", change)

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
//          println(file + ":" + references(file))
          val refs = references(file)
          if (refs.isEmpty)
            invalidate(file, "it is a direct dependency and we don't yet have finer-grained dependency information", change)
          else {
            change match {
              case Removed(Definition(name)) if refs(name) =>
                invalidate(file, "it references deleted definition", change)
              case Removed(Class(name)) if (refs(name)) =>
                invalidate(file, "it references deleted class", change)
              case Changed(Definition(name)) if (refs(name)) =>
                invalidate(file, "it references changed definition", change)
              case _ => ()
            }
          }
        }

        breakable {
          for (file <- directDeps) {
            for (cls <- definitions(file)) checkParents(cls, file)
            for (cls <- definitions(file)) checkInterface(cls, file)
            checkReferences(file)
          }
        }
      }
    }

    buf
  }

  /** Update the map of definitions per source file */
  private def updateDefinitions(files: Set[AbstractFile]) {
    for (src <- files; val localDefs = compiler.dependencyAnalysis.definitions(src)) {
      definitions(src) = (localDefs map (_.cloneSymbol))
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
