import sbt._
import AdditionalResources._

trait Step extends Dag[Step] {
  def dependencies: Iterable[Step]
}

class WrapperStep(contents: List[Step]) extends Step {
  def dependencies = contents
}

abstract class CompilationStep(val name: String, val pathConfig: PathConfig, logger: Logger) extends CompileConfiguration with Step {
  def this(name: String, layout: PathLayout, logger: Logger) = this(name, layout / name, logger)

  // Utility methods (for quick access, ...)
  final def srcDir = pathConfig.sources
  
  // Methods required for the compilation
  def log: Logger = logger
  final def sourceRoots : PathFinder = pathConfig.sources
  def sources: PathFinder  = sourceRoots.descendentsExcept("*.java" | "*.scala", ".svn")
  final def projectPath: Path  = pathConfig.projectRoot
  final def analysisPath: Path = pathConfig.analysis
  final def outputDirectory: Path = pathConfig.output
  def classpath = {
    def addDependenciesOutputTo(list: List[Step], acc: PathFinder): PathFinder =  list match {
      case Nil => acc
      case x :: xs => x match {
        case c: CompilationStep => addDependenciesOutputTo(xs, acc +++ c.outputDirectory)
        case w: WrapperStep => addDependenciesOutputTo(xs, addDependenciesOutputTo(dependencies.toList, acc))
      }
    }
    addDependenciesOutputTo(dependencies.toList, outputDirectory)
  }
  def javaOptions: Seq[String] = "-target 1.5 -source 1.5 -g:none" split ' '
  def maxErrors: Int = 100
  def compileOrder =  CompileOrder.JavaThenScala
  def fingerprints = Fingerprints(Nil, Nil)
}
