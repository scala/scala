import sbt._

/** 
  * An abstract class for grouping all different paths that are needed to
  * compile the a CompilationStep
  * @author Grégory Moix
  */
abstract class PathConfig {
  def projectRoot: Path
  def sources: Path
  def analysis: Path
  def output: Path
}

object PathConfig {
  val classes = "classes"
  val analysis = "analysis"
}

trait SimpleOutputLayout {
  def outputDir: Path
  lazy val classesOutput = outputDir / PathConfig.classes
  lazy val analysisOutput = outputDir / PathConfig.analysis
  
}

class PathLayout(val projectRoot: Path, val outputDir: Path) extends SimpleOutputLayout {
  lazy val srcDir = projectRoot / "src"
  /**
    * An utility method to easily create StandardPathConfig from a given path layout
    */
  def /(name: String)= new StandardPathConfig(this, name)
}

/**
  *
  */
class StandardPathConfig(layout: PathLayout, name: String) extends PathConfig {
  lazy val projectRoot = layout.projectRoot
  lazy val sources = layout.srcDir / name
  lazy val analysis = layout.analysisOutput / name
  lazy val output = layout.classesOutput / name
}
