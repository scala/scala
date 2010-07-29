/*
import sbt._
import java.io.File
import java.net.URLClassLoader

trait PartestRunner{
  self: BasicLayer with Packer =>
  import Partest._
  lazy val testRoot = projectRoot / "test"
  lazy val testFiles = testRoot / "files"
  lazy val testLibs = testFiles / "lib"
  lazy val testSuite= task{
    val config = new TestConfiguration(
      outputLibraryJar,
      (outputLibraryJar +++ outputCompilerJar +++ outputPartestJar +++ outputScalapJar +++ antJar +++ jlineJar +++ (testLibs * "*.jar")).get,
      (testFiles /"pos") * "*.scala",
      (testFiles /"neg") * "*.scala",
      (testFiles / "run") ** "*.scala",
      (testFiles /"jvm") * "*.scala",
      (testFiles / "res") * "*.res",
      (testFiles / "buildmanager"),
      Path.emptyPathFinder,
      (testFiles/ "shootout") * "*.scala",
      (testFiles /"scalap") ** "*.scala"
      )
    val javaHome = Path.fromFile(new File(System.getProperty("java.home")))
    val java = javaHome / "bin" / "java"
    val javac = javaHome / "bin" / "javac"

    runTest(config,Some(java.asFile),Some(javac.asFile),None,Some("2400000"), false,true,false,true,false,log)
  }

}


class TestConfiguration(val library:Path, val classpath:Iterable[Path],
                        posFiles:PathFinder,negFiles:PathFinder,runFiles:PathFinder,  jvmFiles:PathFinder,
                        residentFiles:PathFinder,buildManagerFiles:PathFinder,scriptFiles:PathFinder,
                        shootoutFiles:PathFinder,scalapFiles:PathFinder){


  private def getFilesAndDirs(path:PathFinder):Array[File]={
    ( path * AllPassFilter --- (path * ((new ExactFilter(".svn")) || GlobFilter("*.obj")))).getFiles.toArray
  }

  private def getPosFiles          = getFilesAndDirs(posFiles)
  private def getNegFiles          = getFilesAndDirs(negFiles)
  private def getRunFiles          = getFilesAndDirs(runFiles)
  private def getJvmFiles          = getFilesAndDirs(jvmFiles)
  private def getResidentFiles     = getFilesAndDirs(residentFiles)
  private def getBuildManagerFiles = getFilesAndDirs(buildManagerFiles)
  private def getScriptFiles       = getFilesAndDirs(scriptFiles)
  private def getShootoutFiles     = getFilesAndDirs(shootoutFiles)
  private def getScalapFiles       = getFilesAndDirs(scalapFiles)

  lazy val testFileSets = List(
    (getPosFiles, "pos", "Compiling files that are expected to build"),
    (getNegFiles, "neg", "Compiling files that are expected to fail"),
    (getRunFiles, "run", "Compiling and running files"),
    (getJvmFiles, "jvm", "Compiling and running files"),
    (getResidentFiles, "res", "Running resident compiler scenarii"),
    (getBuildManagerFiles, "buildmanager", "Running Build Manager scenarii"),
    (getScriptFiles, "script", "Running script files"),
    (getShootoutFiles, "shootout", "Running shootout tests"),
    (getScalapFiles, "scalap", "Running scalap tests")
    )


}
*/
/**
 * Based on scala.tools.partest.PartestTask
 */
/*
object Partest{

  def runTest(config:TestConfiguration,javacmd:Option[File],javaccmd:Option[File],scalacOpts:Option[String],timeout:Option[String],
              showDiff:Boolean,showLog:Boolean,runFailed:Boolean,errorOnFailed:Boolean,debug:Boolean,log:Logger):Option[String] = {
    if (debug)
      System.setProperty("partest.debug", "true")

    if (config.classpath.isEmpty)
      return Some("The classpath is empty")


    val classloader = new URLClassLoader(Array(config.classpath.toSeq.map(_.asURL):_*))

    val antRunner: AnyRef =
    classloader.loadClass("scala.tools.partest.nest.AntRunner").newInstance().asInstanceOf[AnyRef]
    val antFileManager: AnyRef =
    antRunner.getClass.getMethod("fileManager", Array[Class[_]](): _*).invoke(antRunner, Array[Object](): _*)

    val runMethod =
    antRunner.getClass.getMethod("reflectiveRunTestsForFiles", Array(classOf[Array[File]], classOf[String]): _*)

    def runTestsForFiles(kindFiles: Array[File], kind: String): (Int, Int) = {
      val result = runMethod.invoke(antRunner, Array(kindFiles, kind): _*).asInstanceOf[Int]
      (result >> 16, result & 0x00FF)
    }

    def setFileManagerBooleanProperty(name: String, value: Boolean) {
      val setMethod =
      antFileManager.getClass.getMethod(name+"_$eq", Array(classOf[Boolean]): _*)
      setMethod.invoke(antFileManager, Array(java.lang.Boolean.valueOf(value)).asInstanceOf[Array[Object]]: _*)
    }

    def setFileManagerStringProperty(name: String, value: String) {
      val setMethod =
      antFileManager.getClass.getMethod(name+"_$eq", Array(classOf[String]): _*)
      setMethod.invoke(antFileManager, Array(value).asInstanceOf[Array[Object]]: _*)
    }

    setFileManagerBooleanProperty("showDiff", showDiff)
    setFileManagerBooleanProperty("showLog", showLog)
    setFileManagerBooleanProperty("failed", runFailed)
    if (!javacmd.isEmpty)
      setFileManagerStringProperty("JAVACMD", javacmd.get.getAbsolutePath)
    if (!javaccmd.isEmpty)
      setFileManagerStringProperty("JAVAC_CMD", javaccmd.get.getAbsolutePath)
    setFileManagerStringProperty("CLASSPATH", config.classpath.mkString(File.pathSeparator))
    setFileManagerStringProperty("LATEST_LIB", config.library.absolutePath)
    if (!scalacOpts.isEmpty)
      setFileManagerStringProperty("SCALAC_OPTS", scalacOpts.get)
    if (!timeout.isEmpty)
      setFileManagerStringProperty("timeout", timeout.get)

    type TFSet = (Array[File], String, String)

    val testFileSets = config.testFileSets

    def runSet(set: TFSet): (Int, Int) = {
      val (files, name, msg) = set
      if (files.isEmpty) (0, 0)
      else {
        log.info(msg)
        runTestsForFiles(files, name)
      }
    }

    val _results = testFileSets map runSet
    val allSuccesses = (_results map (_._1)).foldLeft(0)(_+_)
    val allFailures = (_results map (_._2)).foldLeft(0)(_+_)

    def f(msg:String):Option[String] =
      if (errorOnFailed && allFailures > 0) Some(msg)
      else {
        log.info(msg)
        None
      }
    def s = if (allFailures > 1) "s" else ""
    val msg =
    if (allFailures > 0) "Test suite finished with %d case%s failing.".format(allFailures, s)
    else if (allSuccesses == 0) "There were no tests to run."
    else "Test suite finished with no failures."

    f(msg)
  }


}*/
