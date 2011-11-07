import sbt._
import java.io.File
import java.net.URLClassLoader
import TestSet.{filter}

class TestSet(val SType: TestSetType.Value, val kind: String, val description: String, val files: Array[File]){
  /**
   * @param a list of file that we want to know wheter they are members of the test set or not
   * @return two lists : the first contains files that are member of the test set, the second contains the files that aren't
   */
  def splitContent(f: List[File]):(List[File], List[File]) = {
    f.partition((f: File) => files.elements.exists((e: File) => f == e))
  }
}
    
object TestSet {
    def apply(sType: TestSetType.Value, kind: String, description: String, files: PathFinder)= new TestSet(sType, kind, description, filter(files))
    def filter(p: PathFinder): Array[File] =( p --- p **(HiddenFileFilter || GlobFilter("*.obj")||GlobFilter("*.log"))).getFiles.toArray
}

object TestSetType extends Enumeration {
  val Std, Continuations = Value
}

class TestConfiguration(val library: Path, val classpath: Iterable[Path], val testRoot: Path,
                        val tests: List[TestSet], val junitReportDir: Option[Path]){
}

trait PartestRunner {
  self: BasicLayer with Packer =>

  import Partest.runTest
  import TestSetType._
  
  lazy val testRoot  = projectRoot / "test"
  lazy val testFiles = testRoot / "files" ##
  lazy val testLibs  = testFiles / "lib"

  lazy val posFilesTest          = TestSet(Std,"pos", "Compiling files that are expected to build", testFiles / "pos" * ("*.scala" || DirectoryFilter))
  lazy val negFilesTest          = TestSet(Std,"neg", "Compiling files that are expected to fail", testFiles / "neg" * ("*.scala" || DirectoryFilter))
  lazy val runFilesTest          = TestSet(Std,"run", "Compiling and running files", testFiles / "run" * ("*.scala" || DirectoryFilter))
  lazy val jvmFilesTest          = TestSet(Std,"jvm", "Compiling and running files", testFiles / "jvm" *("*.scala" || DirectoryFilter))
  lazy val resFilesTest          = TestSet(Std,"res", "Running resident compiler scenarii", testFiles / "res" * ("*.res"))
  lazy val buildmanagerFilesTest = TestSet(Std,"buildmanager", "Running Build Manager scenarii", testFiles / "buildmanager" * DirectoryFilter)
  // lazy val scalacheckFilesTest   = TestSet(Std,"scalacheck", "Running scalacheck tests", testFiles / "scalacheck" * ("*.scala" || DirectoryFilter))
  lazy val scriptFilesTest       = TestSet(Std,"script", "Running script files", testFiles / "script" * ("*.scala"))
  lazy val shootoutFilesTest     = TestSet(Std,"shootout", "Running shootout tests", testFiles / "shootout" * ("*.scala"))
  lazy val scalapFilesTest       = TestSet(Std,"scalap", "Running scalap tests", testFiles / "scalap" * ("*.scala"))
  lazy val specializedFilesTest  = TestSet(Std,"specialized", "Running specialized tests", testFiles / "specialized" * ("*.scala"))

  // lazy val negContinuationTest = TestSet(Continuations,"neg", "Compiling continuations files that are expected to fail", testFiles / "continuations-neg" * ("*.scala" || DirectoryFilter))
  // lazy val runContinuationTest = TestSet(Continuations,"run", "Compiling and running continuations files", testFiles / "continuations-run" ** ("*.scala" ))
  // 
  // lazy val continuationScalaOpts = (
  //   "-Xpluginsdir " +
  //   continuationPluginConfig.packagingConfig.jarDestination.asFile.getParent +
  //   " -Xplugin-require:continuations -P:continuations:enable"
  // )

  lazy val testSuiteFiles: List[TestSet] = List(
    posFilesTest, negFilesTest, runFilesTest, jvmFilesTest, resFilesTest, 
    buildmanagerFilesTest, 
    //scalacheckFilesTest, 
    shootoutFilesTest, scalapFilesTest,
    specializedFilesTest
  )
  lazy val testSuiteContinuation: List[TestSet] = Nil // List(negContinuationTest, runContinuationTest)

  private lazy val filesTestMap: Map[String, TestSet] =
    Map(testSuiteFiles.map(s => (s.kind,s) ):_*)
    // + (("continuations-neg",negContinuationTest),("continuations-run", runContinuationTest))

  private lazy val partestOptions = List("-failed")
  
  private lazy val partestCompletionList: Seq[String] = {
    val len = testFiles.asFile.toString.length + 1
    
    filesTestMap.keys.toList ++ partestOptions ++ 
    (filesTestMap.values.toList flatMap (_.files) map (_.toString take len))
  }

  private def runPartest(tests: List[TestSet], scalacOpts: Option[String], failedOnly: Boolean) = {

    val config = new TestConfiguration(
      outputLibraryJar,
      (outputLibraryJar +++ outputCompilerJar +++ outputPartestJar +++ outputScalapJar +++ antJar +++ jlineJar +++  (testLibs * "*.jar")).get,
      testRoot,
      tests,      
      None
    )

    val javaHome = Path.fromFile(new File(System.getProperty("java.home")))
    val java     = Some(javaHome / "bin" / "java" asFile)
    val javac    = Some(javaHome / "bin" / "javac" asFile)
    val timeout  = Some("2400000")
    val loader   = info.launcher.topLoader

    log.debug("Ready to run tests")

    if (tests.isEmpty) {
      log.debug("Empty test list")
      None
    }
    else runTest(
      loader, config, java, javac,
      scalacOpts, timeout, true, true,
      failedOnly, true, isDebug, log
    )
  }
  
  def partestDebugProp =
    if (isDebug) List("-Dpartest.debug=true")
    else Nil
  
  lazy val externalPartest = task { args =>
    task {
      if (isForked) partest(args).run
      else withJVMArgs(partestDebugProp ++ args: _*) {
        if (forkTasks("partest")) None
        else Some("Some tests failed.")
      }
    } dependsOn pack
  } completeWith partestCompletionList

  lazy val partest = task { args =>
    var failedOnly = false

    def setOptions(options: List[String], acc: List[String]): List[String] = options match {
      case "-failed" :: xs =>
        failedOnly = true
        log.info("Only tests that failed previously will be run")
        setOptions(xs, acc)
      case x :: xs =>
        setOptions(xs, x :: acc)
      case _ => acc  
    }
    
    def resolveSets(l: List[String], rem: List[String], acc: List[TestSet]): (List[String], List[TestSet]) = {
      def searchSet(arg: String): Option[TestSet] = filesTestMap get arg

      l match {
        case x :: xs => searchSet(x) match {
          case Some(s) => resolveSets(xs, rem, s :: acc)
          case None => resolveSets(xs, x :: rem, acc)
          }
        case Nil => (rem, acc)
      }
    }
    
    def resolveFiles(l: List[String], sets: List[TestSet]):(List[String], List[TestSet]) = {
      def resolve0(filesToResolve: List[File], setsToSearchIn: List[TestSet], setAcc: List[TestSet]):(List[String], List[TestSet])= {
        filesToResolve match {
          case Nil => (Nil, setAcc) // If we have no files left to resolve, we can return the list of the set we have
          case list => {
            setsToSearchIn match { 
              case Nil => (list.map(_.toString), setAcc)// If we already had search all sets to find a match, we return the list of the files that where problematic and the set we have
              case x :: xs => {
                val (found, notFound)= x.splitContent(list)
                if(!found.isEmpty){
                  val newSet = new TestSet(x.SType, x.kind, x.description, found.toArray)
                  resolve0(notFound, xs, newSet :: setAcc)
                } else {
                  resolve0(notFound, xs, setAcc)
                }
              }
            }
          }
        }
        
      }
      
      resolve0(l.map(Path.fromString(testFiles, _).asFile), filesTestMap.values.toList, sets)
    }

    val keys = setOptions(args.toList, Nil)
    
    if (keys.isEmpty) {
      task { runPartest(testSuiteFiles, None, failedOnly) }
    }
    else {
      val (fileNames, sets)   = resolveSets(keys, Nil, Nil) 
      val (notFound, allSets) = resolveFiles(fileNames, sets)
      if (!notFound.isEmpty)
        log.info("Don't know what to do with : \n"+notFound.mkString("\n"))

      task { runPartest(allSets, None, failedOnly) }
    }
    // if (keys.length == 0) task {
    //   runPartest(testSuiteFiles, None, failedOnly) orElse {
    //     runPartest(testSuiteContinuation, None, failedOnly)
    //   } // this is the case where there were only config options, we will run the standard test suite
    // }
    // else {
    //   val (fileNames, sets)   = resolveSets(keys, Nil, Nil) 
    //   val (notFound, allSets) = resolveFiles(fileNames, sets)
    //   if (!notFound.isEmpty)
    //     log.info("Don't know what to do with : \n"+notFound.mkString("\n"))
    //   
    //   val (std, continuations) = allSets partition (_.SType == TestSetType.Std)
    //   task {
    //     runPartest(std, None, failedOnly) orElse {
    //       runPartest(continuations, Some(continuationScalaOpts), failedOnly)
    //     }
    //   }
    // }
  }.completeWith(partestCompletionList)

}

object Partest {
  def runTest(
    parentLoader: ClassLoader,
    config: TestConfiguration,
    javacmd: Option[File],
    javaccmd: Option[File],
    scalacOpts: Option[String],
    timeout: Option[String],         
    showDiff: Boolean,
    showLog: Boolean,
    runFailed: Boolean,
    errorOnFailed: Boolean,
    debug: Boolean,
    log: Logger
  ): Option[String] = {
    
    if (debug)
      log.setLevel(Level.Debug)
    
    if (config.classpath.isEmpty)
      return Some("The classpath is empty")

    log.debug("Classpath is "+ config.classpath)

    val classloader = new URLClassLoader(
      Array(config.classpath.toSeq.map(_.asURL):_*),
      ClassLoader.getSystemClassLoader.getParent
    )
    val runner: AnyRef =
      classloader.loadClass("scala.tools.partest.nest.SBTRunner").newInstance().asInstanceOf[AnyRef]
    val fileManager: AnyRef =
      runner.getClass.getMethod("fileManager", Array[Class[_]](): _*).invoke(runner, Array[Object](): _*)

    val runMethod =
      runner.getClass.getMethod("reflectiveRunTestsForFiles", Array(classOf[Array[File]], classOf[String]): _*)

    def runTestsForFiles(kindFiles: Array[File], kind: String) = {
      val result = runMethod.invoke(runner, Array(kindFiles, kind): _*).asInstanceOf[java.util.HashMap[String, Int]]
      scala.collection.jcl.Conversions.convertMap(result)
    }

    def setFileManagerBooleanProperty(name: String, value: Boolean) {
      log.debug("Setting partest property :"+name+" to :"+value)
      val setMethod =
        fileManager.getClass.getMethod(name+"_$eq", Array(classOf[Boolean]): _*)
      setMethod.invoke(fileManager, Array(java.lang.Boolean.valueOf(value)).asInstanceOf[Array[Object]]: _*)
    }

    def setFileManagerStringProperty(name: String, value: String) {
      log.debug("Setting partest property :"+name+" to :"+value)      
      val setMethod =
        fileManager.getClass.getMethod(name+"_$eq", Array(classOf[String]): _*)
      setMethod.invoke(fileManager, Array(value).asInstanceOf[Array[Object]]: _*)
    }

    // System.setProperty("partest.srcdir", "files")
    
    setFileManagerBooleanProperty("showDiff", showDiff)
    setFileManagerBooleanProperty("showLog", showLog)
    setFileManagerBooleanProperty("failed", runFailed)
    if (!javacmd.isEmpty)
      setFileManagerStringProperty("JAVACMD", javacmd.get.getAbsolutePath)
    if (!javaccmd.isEmpty)
      setFileManagerStringProperty("JAVAC_CMD", "javac")
    setFileManagerStringProperty("CLASSPATH", (config.classpath.map(_.absolutePath).mkString(File.pathSeparator)))
    setFileManagerStringProperty("LATEST_LIB", config.library.absolutePath)
    setFileManagerStringProperty("SCALAC_OPTS", scalacOpts getOrElse "")

    if (!timeout.isEmpty)
      setFileManagerStringProperty("timeout", timeout.get)

    type TFSet = (Array[File], String, String)

    val testFileSets = config.tests
    
    def resultsToStatistics(results: Iterable[(_, Int)]): (Int, Int) = {
      val (files, failures) = results map (_._2 == 0) partition (_ == true)
      def count(i: Iterable[_]): Int ={
        var c = 0
        for (elem <-i) yield {
            c = c+1
        }
        c
      }
      (count(files), count(failures))
    }
    

    def runSet(set: TestSet): (Int, Int, Iterable[String]) = {
      val (files, name, msg) = (set.files, set.kind, set.description)
      log.debug("["+name+"] "+ msg+files.mkString(", files :\n","\n",""))
      if (files.isEmpty) {
        log.debug("No files !")
        (0, 0, List())
        }
      else {
        log.info(name +" : "+ msg)
        val results: Iterable[(String, Int)] = runTestsForFiles(files, name)
        val (succs, fails) = resultsToStatistics(results)

        val failed: Iterable[String] = results.filter( _._2!=0) map(_ match {
          case (path, 1)    => path + " [FAILED]"
          case (path, 2)    => path + " [TIMOUT]"
        })
        
        val r =(succs, fails, failed)

        config.junitReportDir match {
          case Some(d) => {
            val report = testReport(name, results, succs, fails)
            scala.xml.XML.save(d/name+".xml", report)
          }
          case None => 
        }
        
        r
      }
    }

    val _results = testFileSets map runSet
    val allSuccesses = _results.map (_._1).foldLeft(0)( _ + _ )
    val allFailures = _results.map (_._2).foldLeft(0)( _ + _ )
    val allFailedPaths = _results flatMap (_._3)
    
    def f(msg: String): Option[String] =
      if (errorOnFailed && allFailures > 0) {
        Some(msg)
        }
      else {
        log.info(msg)
        None
      }
    def s = if (allFailures > 1) "s" else ""
    val msg =
    if (allFailures > 0) "Test suite finished with %d case%s failing.\n".format(allFailures, s)+ allFailedPaths.mkString("\n")
    else if (allSuccesses == 0) "There were no tests to run."
    else "Test suite finished with no failures."

    f(msg)
  
  }

  private def oneResult(res: (String, Int)) =
    <testcase name ={res._1}>{
  	  res._2 match {
  	    case 0 => scala.xml.NodeSeq.Empty
        case 1 => <failure message="Test failed"/>
        case 2 => <failure message="Test timed out"/>
  	  } 
  	}</testcase>
   
  private def testReport(kind: String, results: Iterable[(String, Int)], succs: Int, fails: Int) =
    <testsuite name ={kind} tests ={(succs + fails).toString} failures ={fails.toString}>
  	  <properties/>
  	  {
  	    results.map(oneResult(_))
  	  }
    </testsuite>


}
