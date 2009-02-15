package scala.tools.nsc.dependencies;
import util.SourceFile;

trait DependencyAnalysis extends SubComponent with Files{
  import global._

  val phaseName = "dependencyAnalysis";

  def off = settings.make.value == "all"

  def newPhase(prev : Phase) = new AnalysisPhase(prev)

  lazy val maxDepth = settings.make.value match {
    case "changed" => 0
    case "transitive" => Int.MaxValue
    case "immediate" => 1
  }

  def nameToFile(name : Any) =
    settings.outdir.value / (name.toString.replace(".", java.io.File.separator) + ".class")

  lazy val dependenciesFile : Option[File] = settings.dependenciesFile.value match {
    case "none" => None
    case x => Some(toFile(x))
  }

  def classpath = settings.classpath.value
  def newDeps = new FileDependencies(classpath);

  lazy val dependencies =
    dependenciesFile match {
      case Some(f) if f.exists => {
        val fd = FileDependencies.readFrom(f);
        if (fd.classpath != classpath) {
          if(settings.debug.value){
            println("Classpath has changed. Nuking dependencies");
          }
          newDeps
        }
        else fd
      }
      case _ => newDeps;
    }

  def writeToFile() = if(!off){
    dependenciesFile.foreach(dependencies.writeTo(_));
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
        val f = x.path.absolute;
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
        val source : File = unit.source.file.file;
        for (d <- unit.icode){
          dependencies.emits(source, nameToFile(d))
        }

        for (d <- unit.depends; if (d.sourceFile != null)){
          dependencies.depends(source, d.sourceFile.file);
        }
      }
    }
  }
}
