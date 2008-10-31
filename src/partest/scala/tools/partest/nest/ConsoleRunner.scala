/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest.nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter, PrintWriter}

class ConsoleRunner extends DirectRunner with RunnerUtils {

  var fileManager: ConsoleFileManager = _

  private val version = System.getProperty("java.version", "")
  private val isJava5 = version matches "1.[5|6|7].*"

  private var posCheck = false
  private var negCheck = false
  private var runCheck = false
  private var jvmCheck = false
  private var jvm5Check = false
  private var resCheck = false
  private var shootoutCheck = false
  private var scriptCheck = false
  private var scalacheckCheck = false

  private var runAll = false

  private var testFiles: List[File] = List()

  private val errors =
    Integer.parseInt(System.getProperty("scalatest.errors", "0"))

  def denotesTestSet(arg: String) =
    arg match {
      case "--pos"      => true
      case "--neg"      => true
      case "--run"      => true
      case "--jvm"      => true
      case "--jvm5"     => true
      case "--res"      => true
      case "--shootout" => true
      case "--script"   => true
      case "--scalacheck"   => true
      case _            => false
    }

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

      for (arg <- args) {
        arg match {
          case "--all"          => runAll = true

          case "--pos"          => posCheck = true
          case "--neg"          => negCheck = true
          case "--run"          => runCheck = true
          case "--jvm"          => jvmCheck = true
          case "--jvm5"         => jvm5Check = true
          case "--res"          => resCheck = true
          case "--shootout"     => shootoutCheck = true
          case "--script"       => scriptCheck = true
          case "--scalacheck"   => scalacheckCheck = true

          case "--verbose"      => NestUI._verbose = true
          case "--show-diff"    => fileManager.showDiff = true
          case "--show-log"     => fileManager.showLog = true
          case "--failed"       => fileManager.failed = true
          case "--version"      => //todo: printVersion
          case "--ansi"         => NestUI.initialize(NestUI.MANY)
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

      val (successes, failures) = testCheckAll()

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

  def runTests(kind: String, check: Boolean, msg: String): (Int, Int) = {
    if (check) {
      val kindFiles = if (kind == "res") //TODO: is there a nicer way?
        fileManager.getFiles(kind, check, Some((".res", false)))
      else
        fileManager.getFiles(kind, check)
      if (!kindFiles.isEmpty) {
        NestUI.outline("\n"+msg+"\n")
        runTestsForFiles(kindFiles, kind)
      } else {
        NestUI.failure("test dir empty\n")
        (0, 0)
      }
    } else (0, 0)
  }

  /**
   * @return (success count, failure count)
   */
  def testCheckAll(): (Int, Int) = {
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

    if (runAll) { // run all tests
      posCheck = true
      negCheck = true
      runCheck = true
      jvmCheck = true
      jvm5Check = true
      resCheck = true
      shootoutCheck = true
      scriptCheck = true
      scalacheckCheck = true
    }
    val results = List(runTestsFiles,
                       runTests("pos", posCheck, "Testing compiler (on files whose compilation should succeed)"),
                       runTests("neg", negCheck, "Testing compiler (on files whose compilation should fail)"),
                       runTests("run", runCheck, "Testing JVM backend"),
                       runTests("jvm", jvmCheck, "Testing JVM backend"),
                       runTests("jvm5", jvm5Check, "Testing JVM backend"),
                       runTests("res", resCheck, "Testing resident compiler"),
                       runTests("shootout", shootoutCheck, "Testing shootout tests"),
                       runTests("script", scriptCheck, "Testing script tests"),
                       runTests("scalacheck", scalacheckCheck, "Testing ScalaCheck tests"))
    results reduceLeft { (p: (Int, Int), q: (Int, Int)) =>
      (p._1+q._1, p._2+q._2) }
  }
}
