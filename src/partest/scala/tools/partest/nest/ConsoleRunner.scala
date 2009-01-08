/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest.nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter, PrintWriter}

class ConsoleRunner extends DirectRunner with RunnerUtils {

  case class TestSet(loc: String,
                     filter: Option[(String, Boolean)],
                     kind: String,
                     msg: String)

  val testSets = {
    val fileFilter = Some((".scala", true))
    List(
      TestSet("pos", fileFilter, "pos",
              "Testing compiler (on files whose compilation should succeed)"),
      TestSet("pos5", fileFilter, "pos",
              "Testing compiler (on files whose compilation should succeed on 1.5 JVM)"),
      TestSet("neg",  fileFilter, "neg",
              "Testing compiler (on files whose compilation should fail)"),
      TestSet("run",  fileFilter, "run", "Testing JVM backend"),
      TestSet("jvm",  fileFilter, "jvm", "Testing JVM backend"),
      TestSet("jvm5", fileFilter, "jvm5", "Testing JVM backend"),
      TestSet("res",  Some((".res", false)), "res",
              "Testing resident compiler"),
      TestSet("shootout", fileFilter, "shootout", "Testing shootout tests"),
      TestSet("script", fileFilter, "script", "Testing script tests"),
      TestSet("scalacheck", fileFilter, "scalacheck", "Testing ScalaCheck tests"))
  }

  var fileManager: ConsoleFileManager = _

  private val version = System.getProperty("java.version", "")
  private val isJava5 = version matches "1.[5|6|7].*"
  private var runAll = false
  private var testFiles: List[File] = List()
  private val errors =
    Integer.parseInt(System.getProperty("scalatest.errors", "0"))

  def denotesTestSet(arg: String) =
    testSets exists { set => arg == "--" + set.loc }

  def denotesTestFile(arg: String) =
    arg.endsWith(".scala") || arg.endsWith(".res")

  def denotesTestDir(arg: String) =
    (new File(arg)).isDirectory

  def main(argstr: String) {
    // tokenize args. filter: "".split("\\s") yields Array("")
    var args = List.fromArray(argstr.split("\\s")).remove(_ == "")

    if (args.length == 0)
      NestUI.usage()
    else {
      // find out which build to test
      val (buildPath, args1) = searchAndRemovePath("--buildpath", args)
      val (classPath, args2) = searchAndRemovePath("--classpath", args1)
      val (srcPath, args3) = searchAndRemovePath("--srcpath", args2)
      args = args3

      if (!srcPath.isEmpty)
        System.setProperty("partest.srcdir", srcPath.get)

      fileManager =
        if (!buildPath.isEmpty)
          new ConsoleFileManager(buildPath.get)
        else if (!classPath.isEmpty)
          new ConsoleFileManager(classPath.get, true)
        else if (args contains "--pack") {
          args = args.remove(_ == "--pack") // will create a result file '--pack' otherwise
          new ConsoleFileManager("build/pack")
        } else if (args contains "--four") {
          args = args.remove(_ == "--four")
          new ConsoleFileManager("build/four-pack", false, "-target:jvm-1.4")
        } else // auto detection, see ConsoleFileManager.findLatest
          new ConsoleFileManager

      if (!args.exists(denotesTestSet(_)) &&
          !args.exists(denotesTestFile(_)) &&
          !args.exists(denotesTestDir(_)))
        runAll = true

      var enabled = List[TestSet]()
      var readTimeout = false
      for (arg <- args) {
        (testSets find { set => arg == "--" + set.loc }) match {
          case Some(set) => enabled = set :: enabled
          case None      => arg match {
            case "--all"          => runAll = true
            case "--verbose"      => NestUI._verbose = true
            case "--show-diff"    => fileManager.showDiff = true
            case "--show-log"     => fileManager.showLog = true
            case "--failed"       => fileManager.failed = true
            case "--version"      => //todo: printVersion
            case "--ansi"         => NestUI.initialize(NestUI.MANY)
            case "--timeout"      => readTimeout = true
            case s: String if readTimeout =>
              fileManager.timeout = s
              readTimeout = false
            case _ =>
              if (denotesTestFile(arg) || denotesTestDir(arg)) {
                val file = new File(arg)
                if (file.exists) {
                  NestUI.verbose("adding test file "+file)
                  testFiles = file :: testFiles
                } else {
                  NestUI.failure("File \"" + arg + "\" not found\n")
                  System.exit(1)
                }
              } else {
                NestUI.failure("Invalid option \""+arg+"\"\n")
                NestUI.usage()
              }
          }
        }
      }
      NestUI.verbose("enabled test sets: "+enabled)
      NestUI.verbose("runAll: "+runAll)

      val dir =
        if (!fileManager.testClasses.isEmpty)
          fileManager.testClassesFile
        else if (fileManager.testBuild != null)
          fileManager.testBuildFile
        else
          fileManager.latestCompFile.getParentFile.getParentFile.getCanonicalFile
      NestUI.outline("Scala compiler classes in: "+dir+"\n")

      val scalaVersion = "Scala compiler "+
        scala.tools.nsc.Properties.versionString+
        " -- "+
        scala.tools.nsc.Properties.copyrightString

      NestUI.outline("Scala version is:          "+scalaVersion+"\n")
      NestUI.outline("Scalac options are:        "+fileManager.SCALAC_OPTS+"\n")

      val vmBin  = System.getProperty("java.home", "")+File.separator+"bin"
      val vmName = System.getProperty("java.vm.name", "")+" (build "+
                   System.getProperty("java.vm.version", "")+", "+
                   System.getProperty("java.vm.info", "")+")"
      val vmOpts = fileManager.JAVA_OPTS
      NestUI.outline("Java binaries in:          "+vmBin+"\n")
      NestUI.outline("Java runtime is:           "+vmName+"\n")
      NestUI.outline("Java options are:          "+vmOpts+"\n")
      NestUI.outline("Source directory is:       "+fileManager.srcDir.getAbsolutePath+"\n")

      val start = System.currentTimeMillis

      val (successes, failures) = testCheckAll(enabled)

      val end = System.currentTimeMillis
      val total = successes + failures

      val elapsedSecs = (end - start)/1000
      val elapsedMins = elapsedSecs/60
      val elapsedHrs  = elapsedMins/60
      val dispMins = elapsedMins - elapsedHrs  * 60
      val dispSecs = elapsedSecs - elapsedMins * 60
      val dispElapsed = {
        def form(num: Long) = if (num < 10) "0"+num else ""+num
        form(elapsedHrs)+":"+form(dispMins)+":"+form(dispSecs)
      }

      println
      if (failures == 0)
        NestUI.success("All of "+total+" tests were successful (elapsed time: "+dispElapsed+")\n")
      else
        NestUI.failure(failures+" of "+total+" tests failed (elapsed time: "+dispElapsed+")\n")

      if (failures == errors)
        System.exit(0)
      else
        System.exit(1)
    }
  }

  def runTests(testSet: TestSet): (Int, Int) = {
    val TestSet(loc, filter, kind, msg) = testSet
    val files = fileManager.getFiles(loc, true, filter)
    if (!files.isEmpty) {
      NestUI.outline("\n"+msg+"\n")
      runTestsForFiles(files, kind)
    } else {
      NestUI.failure("test dir empty\n")
      (0, 0)
    }
  }

  /**
   * @return (success count, failure count)
   */
  def testCheckAll(enabledSets: List[TestSet]): (Int, Int) = {
    def runTestsFiles = if (!testFiles.isEmpty) {
      def absName(f: File): String = f.getAbsoluteFile.getCanonicalPath

      def kindOf(f: File): String = {
        val firstName = absName(f)
        val len = fileManager.srcDirName.length
        val filesPos = firstName.indexOf(fileManager.srcDirName)
        if (filesPos == -1) {
          NestUI.failure("invalid test file: "+firstName+"\n")
          Predef.exit(1)
        } else {
          val k = firstName.substring(filesPos+len+1, filesPos+len+1+3)
          val short = if (k == "jvm") {
            if (firstName.substring(filesPos+len+1, filesPos+len+1+4) == "jvm5") "jvm5"
            else k
          } else k
          val shortKinds = List("pos", "neg", "run", "jvm", "jvm5", "res")
          if (shortKinds contains short) short
          else short match {
            case "sho" => "shootout"
            case "scr" => "script"
            case "sca" => "scalacheck"
          }
        }
      }

      val fstKind = kindOf(testFiles.head)
      NestUI.verbose("all test files expected to have kind "+fstKind)
      if (!testFiles.forall(kindOf(_) equals fstKind)) {
        NestUI.failure("test files have different kinds\n")
        Predef.exit(1)
      } else {
        NestUI.outline("\nTesting individual files\n")
        runTestsForFiles(testFiles, fstKind)
      }
    } else (0, 0)

    val runSets =
      if (runAll) testSets // run all test sets
      else enabledSets
    NestUI.verbose("run sets: "+runSets)

    val results = List(runTestsFiles) ::: (runSets map runTests)
    results reduceLeft { (p: (Int, Int), q: (Int, Int)) =>
      (p._1+q._1, p._2+q._2) }
  }
}
