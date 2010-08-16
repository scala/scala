import sbt._

/**
  * An abstract class for grouping all different paths that are needed to
  * compile the a CompilationStep
  * @author Grégory Moix
  */
abstract class PathConfig {
  def projectRoot:Path
  def sources:Path
  def analysis:Path
  def output:Path
}

/**
  *
  */

class PathLayout(val projectRoot:Path, val outputDir:Path) {
  lazy val srcDir = projectRoot / "src"
  lazy val classesOutput = outputDir / "classes"
  lazy val analysisOutput = outputDir / "analysis"

  /**
    * An utility method to easily create StandardPathConfig from a given path layout
    */
  def /(name:String)= new StandardPathConfig(this, name)
}

/**
  *
  */
class StandardPathConfig(layout: PathLayout, name:String) extends PathConfig{
  lazy val projectRoot = layout.projectRoot
  lazy val sources = layout.srcDir / name
  lazy val analysis = layout.analysisOutput / name
  lazy val output = layout.classesOutput / name
}
