package scala.tools.nsc
package dependencies

import io.Path
import collection._
import symtab.Flags
import scala.tools.nsc.io.AbstractFile
import scala.reflect.internal.util.SourceFile

trait DependencyAnalysis extends SubComponent with Files {
  import global._

  val phaseName = "dependencyAnalysis"

  def off                  = settings.make.isDefault || settings.make.value == "all"
  def shouldCheckClasspath = settings.make.value != "transitivenocp"

  def newPhase(prev: Phase) = new AnalysisPhase(prev)

  private def depPath = Path(settings.dependenciesFile.value)
  def loadDependencyAnalysis(): Boolean = (
    depPath.path != "none" && depPath.isFile && loadFrom(
      AbstractFile.getFile(depPath),
      path => AbstractFile.getFile(depPath.parent resolve Path(path))
    )
  )
  def saveDependencyAnalysis(): Unit = {
    if (!depPath.exists)
      dependenciesFile = AbstractFile.getFile(depPath.createFile())

    /** The directory where file lookup should start */
    val rootPath = depPath.parent.normalize
    saveDependencies(
      file => rootPath.relativize(Path(file.file).normalize).path
    )
  }

  lazy val maxDepth = settings.make.value match {
    case "changed"   => 0
    case "immediate" => 1
    case _           => Int.MaxValue
  }

  // todo: order insensible checking and, also checking timestamp?
  def validateClasspath(cp1: String, cp2: String): Boolean = cp1 == cp2

  def nameToFile(src: AbstractFile, name: String) =
    settings.outputDirs.outputDirFor(src)
      .lookupPathUnchecked(name.toString.replace(".", java.io.File.separator) + ".class", false)

  private var depFile: Option[AbstractFile] = None

  def dependenciesFile_=(file: AbstractFile) {
    assert(file ne null)
    depFile = Some(file)
  }

  def dependenciesFile: Option[AbstractFile] = depFile

  def classpath = settings.classpath.value
  def newDeps = new FileDependencies(classpath)

  var dependencies = newDeps

  def managedFiles = dependencies.dependencies.keySet

  /** Top level definitions per source file. */
  val definitions: mutable.Map[AbstractFile, List[Symbol]] =
    new mutable.HashMap[AbstractFile, List[Symbol]] {
      override def default(f: AbstractFile) = Nil
  }

  /** External references used by source file. */
  val references: mutable.Map[AbstractFile, immutable.Set[String]] =
    new mutable.HashMap[AbstractFile, immutable.Set[String]] {
      override def default(f: AbstractFile) = immutable.Set()
    }

  /** External references for inherited members used in the source file */
  val inherited: mutable.Map[AbstractFile, immutable.Set[Inherited]] =
    new mutable.HashMap[AbstractFile, immutable.Set[Inherited]] {
      override def default(f: AbstractFile) = immutable.Set()
    }

  /** Write dependencies to the current file. */
  def saveDependencies(fromFile: AbstractFile => String) =
    if(dependenciesFile.isDefined)
      dependencies.writeTo(dependenciesFile.get, fromFile)

  /** Load dependencies from the given file and save the file reference for
   *  future saves.
   */
  def loadFrom(f: AbstractFile, toFile: String => AbstractFile): Boolean = {
    dependenciesFile = f
    FileDependencies.readFrom(f, toFile) match {
      case Some(fd) =>
        val success = if (shouldCheckClasspath) validateClasspath(fd.classpath, classpath) else true
        dependencies = if (success) fd else {
          if (settings.debug.value)
            println("Classpath has changed. Nuking dependencies")
          newDeps
        }

        success
      case None => false
    }
  }

  def calculateFiles(files: List[SourceFile]): List[SourceFile] =
    if (off) files
    else if (dependencies.isEmpty) {
      println("No known dependencies. Compiling " +
              (if (settings.debug.value) files.mkString(", ") else "everything"))
      files
    } else {
      val (direct, indirect) = dependencies.invalidatedFiles(maxDepth);
      val filtered = files.filter(x => {
        val f = x.file.absolute
        direct(f) || indirect(f) || !dependencies.containsFile(f);
      })
      filtered match {
        case Nil => println("No changes to recompile");
        case x => println("Recompiling " + (
          if(settings.debug.value) x.mkString(", ") else x.length + " files")
        )
      }
      filtered
    }

  case class Inherited(qualifier: String, member: Name)

  class AnalysisPhase(prev: Phase) extends StdPhase(prev) {

    override def cancelled(unit: CompilationUnit) =
      super.cancelled(unit) && !unit.isJava

    def apply(unit : global.CompilationUnit) {
      val f = unit.source.file.file
      // When we're passed strings by the interpreter
      // they  have no source file. We simply ignore this case
      // as irrelevant to dependency analysis.
      if (f != null){
        val source: AbstractFile = unit.source.file;
        for (d <- unit.icode){
          val name = d.toString
          d.symbol match {
            case s : ModuleClassSymbol =>
              val isTopLevelModule = afterPickler { !s.isImplClass && !s.isNestedClass }

              if (isTopLevelModule && (s.companionModule != NoSymbol)) {
                dependencies.emits(source, nameToFile(unit.source.file, name))
              }
              dependencies.emits(source, nameToFile(unit.source.file, name + "$"))
            case _ =>
              dependencies.emits(source, nameToFile(unit.source.file, name))
          }
        }

        dependencies.reset(source)
        for (d <- unit.depends; if (d.sourceFile != null)){
          dependencies.depends(source, d.sourceFile)
        }
      }

      // find all external references in this compilation unit
      val file = unit.source.file
      references += file -> immutable.Set.empty[String]
      inherited += file -> immutable.Set.empty[Inherited]

      val buf = new mutable.ListBuffer[Symbol]

      (new Traverser {
        override def traverse(tree: Tree) {
          if ((tree.symbol ne null)
              && (tree.symbol != NoSymbol)
              && (!tree.symbol.isPackage)
              && (!tree.symbol.isJavaDefined)
              && (!tree.symbol.tpe.isError)
              && ((tree.symbol.sourceFile eq null)
                  || (tree.symbol.sourceFile.path != file.path))
              && (!tree.symbol.isClassConstructor)) {
            updateReferences(tree.symbol.fullName)
            // was "at uncurryPhase.prev", which is actually non-deterministic
            // because the continuations plugin may or may not supply uncurry's
            // immediately preceding phase.
            beforeRefchecks(checkType(tree.symbol.tpe))
          }

          tree match {
            case cdef: ClassDef if !cdef.symbol.hasPackageFlag &&
                                   !cdef.symbol.isAnonymousFunction =>
              if (cdef.symbol != NoSymbol) buf += cdef.symbol
              // was "at erasurePhase.prev"
              beforeExplicitOuter {
                for (s <- cdef.symbol.info.decls)
                  s match {
                    case ts: TypeSymbol if !ts.isClass =>
                      checkType(s.tpe)
                    case _ =>
                  }
              }
              super.traverse(tree)

            case ddef: DefDef =>
              // was "at typer.prev"
              beforeTyper { checkType(ddef.symbol.tpe) }
              super.traverse(tree)
            case a @ Select(q, n) if ((a.symbol != NoSymbol) && (q.symbol != null)) => // #2556
              if (!a.symbol.isConstructor &&
                  !a.symbol.owner.isPackageClass &&
                  !isSameType(q.tpe, a.symbol.owner.tpe))
                  inherited += file ->
                    (inherited(file) + Inherited(q.symbol.tpe.resultType.safeToString, n))
              super.traverse(tree)
            case _            =>
              super.traverse(tree)
          }
        }

        def checkType(tpe: Type): Unit =
          tpe match {
            case t: MethodType =>
              checkType(t.resultType)
              for (s <- t.params) checkType(s.tpe)

            case t: TypeRef    =>
              if (t.sym.isAliasType) {
                  updateReferences(t.typeSymbolDirect.fullName)
                  checkType(t.typeSymbolDirect.info)
              }
              updateReferences(t.typeSymbol.fullName)
              for (tp <- t.args) checkType(tp)

            case t: PolyType   =>
              checkType(t.resultType)
              updateReferences(t.typeSymbol.fullName)

            case t: NullaryMethodType =>
              checkType(t.resultType)
              updateReferences(t.typeSymbol.fullName)

            case t             =>
              updateReferences(t.typeSymbol.fullName)
          }

        def updateReferences(s: String): Unit =
          references += file -> (references(file) + s)

      }).apply(unit.body)

      definitions(unit.source.file) = buf.toList
    }
  }
}
