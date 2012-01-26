package scala.tools.partest
package nest

import java.io.File
import scala.tools.nsc.io.{ Directory }
import scala.util.Properties.setProp


object SBTRunner extends DirectRunner {

  val fileManager = new FileManager {
    var JAVACMD: String        = "java"
    var JAVAC_CMD: String      = "javac"
    var CLASSPATH: String      = _
    var LATEST_LIB: String     = _
    var LATEST_COMP: String     = _
    var LATEST_PARTEST: String     = _
    val testRootPath: String   = "test"
    val testRootDir: Directory = Directory(testRootPath)
  }

  def reflectiveRunTestsForFiles(kindFiles: Array[File], kind: String):java.util.HashMap[String,Int] = {
    def convert(scalaM:scala.collection.immutable.Map[String,Int]):java.util.HashMap[String,Int] = {
      val javaM = new java.util.HashMap[String,Int]()
      for(elem <- scalaM) yield {javaM.put(elem._1,elem._2)}
      javaM
    }

    def failedOnlyIfRequired(files:List[File]):List[File]={
      if (fileManager.failed) files filter (x => fileManager.logFileExists(x, kind)) else files
    }
    convert(runTestsForFiles(failedOnlyIfRequired(kindFiles.toList), kind))
  }

  case class CommandLineOptions(classpath: Option[String] = None,
                                tests: Map[String, Array[File]] = Map(),
                                scalacOptions: Seq[String] = Seq(),
                                justFailedTests: Boolean = false)

  def mainReflect(args: Array[String]): java.util.Map[String,Int] = {
    setProp("partest.debug", "true")
    setProperties()

    val Argument = new scala.util.matching.Regex("-(.*)")
    def parseArgs(args: Seq[String], data: CommandLineOptions): CommandLineOptions = args match {
      case Seq("--failed", rest @ _*)               => parseArgs(rest, data.copy(justFailedTests = true))
      case Seq("-cp", cp, rest @ _*)                => parseArgs(rest, data.copy(classpath=Some(cp)))
      case Seq("-scalacoption", opt, rest @ _*)     => parseArgs(rest, data.copy(scalacOptions= data.scalacOptions :+ opt))
      case Seq(Argument(name), runFiles, rest @ _*) => parseArgs(rest, data.copy(tests=data.tests + (name -> runFiles.split(",").map(new File(_)))))
      case Seq()                                    => data
      case x                                        => sys.error("Unknown command line options: " + x)
    }
    val config = parseArgs(args, CommandLineOptions())
    fileManager.SCALAC_OPTS = config.scalacOptions
    fileManager.CLASSPATH = config.classpath getOrElse sys.error("No classpath set")
    // Find scala library jar file...
    val lib: Option[String] = (fileManager.CLASSPATH split File.pathSeparator filter (_ matches ".*scala-library.*\\.jar")).headOption
    fileManager.LATEST_LIB = lib getOrElse sys.error("No scala-library found! Classpath = " + fileManager.CLASSPATH)
    val comp: Option[String] = (fileManager.CLASSPATH split File.pathSeparator filter (_ matches ".*scala-compiler.*\\.jar")).headOption
    fileManager.LATEST_COMP = comp getOrElse sys.error("No scala-compiler found! Classpath = " + fileManager.CLASSPATH)
    val partest: Option[String] = (fileManager.CLASSPATH split File.pathSeparator filter (_ matches ".*scala-partest.*\\.jar")).headOption
    fileManager.LATEST_PARTEST = partest getOrElse sys.error("No scala-partest found! Classpath = " + fileManager.CLASSPATH)
    // TODO - Do something useful here!!!
    fileManager.JAVAC_CMD = "javac"
    fileManager.failed      = config.justFailedTests
    // TODO - Make this a flag?
    //fileManager.updateCheck = true
    // Now run and report...
    val runs = config.tests.filterNot(_._2.isEmpty)
    // This next bit uses java maps...
    import collection.JavaConverters._
    (for {
     (testType, files) <- runs
     (path, result) <- reflectiveRunTestsForFiles(files,testType).asScala
    } yield (path, result)).seq asJava
  }

  def main(args: Array[String]): Unit = {
    import collection.JavaConverters._
    val failures = (
      for ((path, result) <- mainReflect(args).asScala ; if result == 1 || result == 2) yield
        path + ( if (result == 1) " [FAILED]" else " [TIMEOUT]" )
    )
    // Re-list all failures so we can go figure out what went wrong.
    failures foreach System.err.println
    if(!failures.isEmpty) sys.exit(1)
  }
}
