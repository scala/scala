package scala.tools.nsc
package dependencies;
import util.SourceFile;
import io.AbstractFile
import collection._
import symtab.Flags

trait DependencyAnalysis extends SubComponent with Files {
  import global._

  val phaseName = "dependencyAnalysis";

  def off = settings.make.value == "all"

  def newPhase(prev : Phase) = new AnalysisPhase(prev)

  lazy val maxDepth = settings.make.value match {
    case "changed" => 0
    case "transitive" => Int.MaxValue
    case "immediate" => 1
  }

  def nameToFile(src: AbstractFile, name : String) =
    settings.outputDirs.outputDirFor(src)
      .lookupPathUnchecked(name.toString.replace(".", java.io.File.separator) + ".class", false)

  private var depFile: Option[AbstractFile] = None

  def dependenciesFile_=(file: AbstractFile) {
    assert(file ne null)
    depFile = Some(file)
  }

  def dependenciesFile: Option[AbstractFile] = depFile

  def classpath = settings.classpath.value
  def newDeps = new FileDependencies(classpath);

  var dependencies = newDeps

  def managedFiles = dependencies.dependencies.keySet

  /** Top level definitions per source file. */
  val definitions: mutable.Map[AbstractFile, List[Symbol]] =
    new mutable.HashMap[AbstractFile, List[Symbol]] {
      override def default(f : AbstractFile) = Nil
  }

  /** External references used by source file. */
  val references: mutable.Map[AbstractFile, immutable.Set[String]] =
    new mutable.HashMap[AbstractFile, immutable.Set[String]] {
      override def default(f : AbstractFile) = immutable.Set()
    }

  /** Write dependencies to the current file. */
  def saveDependencies(fromFile: AbstractFile => String) =
    if(dependenciesFile.isDefined)
      dependencies.writeTo(dependenciesFile.get, fromFile)

  /** Load dependencies from the given file and save the file reference for
   *  future saves.
   */
  def loadFrom(f: AbstractFile, toFile: String => AbstractFile) : Boolean = {
    dependenciesFile = f
    FileDependencies.readFrom(f, toFile) match {
      case Some(fd) =>
        val success = fd.classpath == classpath
        dependencies = if (success) fd else {
          if(settings.debug.value){
            println("Classpath has changed. Nuking dependencies");
          }
          newDeps
        }

        success
      case None => false
    }
  }

  def filter(files : List[SourceFile]) : List[SourceFile] =
    if (off) files
    else if (dependencies.isEmpty){
      if(settings.debug.value){
        println("No known dependencies. Compiling everything");
      }
      files
    }
    else {
      val (direct, indirect) = dependencies.invalidatedFiles(maxDepth);
      val filtered = files.filter(x => {
        val f = x.file.absolute
        direct(f) || indirect(f) || !dependencies.containsFile(f);
      })
      filtered match {
        case Nil => println("No changes to recompile");
        case x => println("Recompiling " + (
          if(settings.debug.value) x.mkString(", ")
          else x.length + " files")
        )
      }
      filtered
    }

  class AnalysisPhase(prev : Phase) extends StdPhase(prev){
    def apply(unit : global.CompilationUnit) {
      val f = unit.source.file.file;
      // When we're passed strings by the interpreter
      // they  have no source file. We simply ignore this case
      // as irrelevant to dependency analysis.
      if (f != null){
        val source: AbstractFile = unit.source.file;
        for (d <- unit.icode){
          val name = d.toString
          dependencies.emits(source, nameToFile(unit.source.file, name))
          d.symbol match {
            case _ : ModuleClassSymbol =>
              dependencies.emits(source, nameToFile(unit.source.file, name + "$"))
            case _ =>
          }
        }

        for (d <- unit.depends; if (d.sourceFile != null)){
          dependencies.depends(source, d.sourceFile);
        }
      }

      // find all external references in this compilation unit
      val file = unit.source.file
      references += file -> immutable.Set.empty[String]

      val buf = new mutable.ListBuffer[Symbol]

      (new Traverser {
        override def traverse(tree: Tree) {
          if ((tree.symbol ne null)
              && (tree.symbol != NoSymbol)
              && (!tree.symbol.isPackage)
              && (!tree.symbol.hasFlag(Flags.JAVA))
              && ((tree.symbol.sourceFile eq null)
                  || (tree.symbol.sourceFile.path != file.path))) {
            references += file -> (references(file) + tree.symbol.fullNameString)
          }
          tree match {
            case cdef: ClassDef if !cdef.symbol.isModuleClass && !cdef.symbol.hasFlag(Flags.PACKAGE) =>
              buf += cdef.symbol
              super.traverse(tree)

            case _ =>
              super.traverse(tree)
          }
        }
      }).apply(unit.body)

      definitions(unit.source.file) = buf.toList
    }
  }
}
