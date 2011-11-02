import sbt._
import ScalaBuildProject._


abstract class ScalaBuildProject extends Project {
  lazy val projectRoot = info.projectPath  
  lazy val layerOutput = outputRootPath / name
  lazy val pathLayout = new PathLayout(projectRoot, layerOutput)
  
  lazy val manifestPath = projectRoot/"META-INF"/"MANIFEST.MF"

  lazy val lib = projectRoot / "lib"
  lazy val forkJoinJar = lib / forkjoinJarName
  lazy val jlineJar = lib / jlineJarName
  lazy val antJar = lib / "ant" / "ant.jar"
  lazy val fjbgJar = lib / fjbgJarName
  lazy val msilJar = lib /  msilJarName

}

object ScalaBuildProject {
   // Some path definitions related strings
  val compilerJarName = "scala-compiler.jar"
  val libraryJarName = "scala-library.jar"
  val scalacheckJarName = "scalacheck.jar"
  val scalapJarName = "scalap.jar"
  val dbcJarName = "scala-dbc.jar"
  val swingJarName = "scala-swing.jar"
  val partestJarName = "scala-partest.jar"
  val fjbgJarName = "fjbg.jar"
  val msilJarName = "msil.jar"
  val jlineJarName = "jline.jar"
  val forkjoinJarName = "forkjoin.jar"


}
