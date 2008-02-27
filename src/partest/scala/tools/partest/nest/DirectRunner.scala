/* NEST (New Scala Test)
 * Copyright 2007-2008 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id: $

package scala.tools.partest.nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter, PrintWriter}
import java.util.StringTokenizer

import scala.actors.Actor._

class DirectRunner {
  private val version = System.getProperty("java.version", "")
  private val isJava5 = version matches "1.[5|6|7].*"

  private val numActors = Integer.parseInt(System.getProperty("scalatest.actors", "8"))

  private var posCheck = false
  private var negCheck = false
  private var runCheck = false
  private var jvmCheck = false
  private var jvm5Check = false
  private var resCheck = false
  private var runAll = false

  private var testFiles: List[File] = List()
  private val con = new PrintStream(Console.out)
  private var out = con

  private val errors =
    Integer.parseInt(System.getProperty("scalatest.errors", "0"))

  def denotesTestSet(arg: String) =
    arg match {
      case "--pos"  => true
      case "--neg"  => true
      case "--run"  => true
      case "--jvm"  => true
      case "--jvm5" => true
      case "--res"  => true
      case _        => false
    }

  def main(argstr: String) {
    // tokenize args
    var args: List[String] = List()
    val st = new StringTokenizer(argstr)
    while (st.hasMoreTokens) {
      args = args ::: List(st.nextToken())
    }

    if (args.length == 0)
      NestUI.usage()
    else {
      if (!args.exists(denotesTestSet(_))) runAll = true
      for (arg <- args) {
        arg match {
          case "--pos"          => posCheck = true
          case "--neg"          => negCheck = true
          case "--run"          => runCheck = true
          case "--jvm"          => jvmCheck = true
          case "--jvm5"         => jvm5Check = true
          case "--res"          => resCheck = true
          case "--verbose"      => NestUI._verbose = true
          case "--show-diff"    => FileManager.showDiff = true
          case "--show-log"     => FileManager.showLog = true
          case "--failed"       => FileManager.failed = true
          case "--version"      => //todo: printVersion
          case "--ansi"         => NestUI.initialize(NestUI.MANY)
          case _ =>
            if (arg endsWith ".scala") {
              val file = new File(arg)
              if (file.isFile) {
                NestUI.verbose("adding test file "+file)
                testFiles = file :: testFiles
              } else {
                NestUI.failure("File \"" + arg + "\" not found")
                System.exit(1)
              }
            } else if (out eq con) {
              val file = new File(arg)
              if (file.isFile || file.createNewFile)
                out = new PrintStream(new FileOutputStream(file))
              else {
                NestUI.failure("Result file \"" + arg + "\" not found")
                System.exit(1)
              }
            } else
              NestUI.usage()
        }
      }

      NestUI.outline("Source directory is : "+FileManager.srcDir.getAbsolutePath+"\n")
      NestUI.outline("Scala binaries in   : "+FileManager.BIN_DIR+"\n")

      // obtain scalac version
      val cmd = FileManager.SCALAC_CMD+" -version"
      NestUI.verbose("running "+cmd)
      val proc = Runtime.getRuntime.exec(cmd)
      val in = proc.getInputStream
      val err = proc.getErrorStream
      val exitCode = proc.waitFor()
      NestUI.verbose("exit code: "+exitCode)
      val scalaVersion = StreamAppender.appendToString(in, err)

      NestUI.outline("Scala version is    : "+scalaVersion)
      NestUI.outline("Scalac options are  : "+FileManager.SCALAC_OPTS+"\n")

      val vmBin  = System.getProperty("java.home", "")+File.separator+"bin"
      val vmName = System.getProperty("java.vm.name", "")+" (build "+
                   System.getProperty("java.vm.version", "")+", "+
                   System.getProperty("java.vm.info", "")+")"
      val vmOpts = System.getProperty("scalatest.java_options", "?")
      NestUI.outline("Java binaries in    : "+vmBin+"\n")
      NestUI.outline("Java runtime is     : "+vmName+"\n")
      NestUI.outline("Java options are    : "+vmOpts+"\n")

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
      val fileMgr = new FileManager
      val kindFiles =
        if (!testFiles.isEmpty) {
          NestUI.verbose("testing "+testFiles)
          testFiles
        }
        else if (kind == "res") //TODO: is there a nicer way?
          fileMgr.getFiles(kind, check, ".res")
        else
          fileMgr.getFiles(kind, check)
      if (!kindFiles.isEmpty) {
        NestUI.outline("\n"+msg+"\n")

        val len = kindFiles.length
        val (testsEach, lastFrag) = (len/numActors, len%numActors)
        val last = numActors-1
        val workers = for (i <- List.range(0, numActors)) yield {
          val toTest = kindFiles.slice(i*testsEach, (i+1)*testsEach)
          val worker = new Worker
          worker.start()
          if (i == last)
            worker ! RunTests(kind, (kindFiles splitAt (last*testsEach))._2)
          else
            worker ! RunTests(kind, toTest)
          worker
        }
        var succs = 0; var fails = 0
        var logsToDelete: List[File] = List()
        var outdirsToDelete: List[File] = List()
        workers foreach { w =>
          receive {
            case Results(s, f, logs, outdirs) =>
              logsToDelete = logsToDelete ::: logs.filter(_.toDelete)
              outdirsToDelete = outdirsToDelete ::: outdirs
              succs += s
              fails += f
          }
        }
        logsToDelete.foreach { log =>
          NestUI.verbose("deleting "+log+"\n")
          FileManager.deleteRecursive(log)
                            }
        outdirsToDelete.foreach { outdir =>
          NestUI.verbose("deleting "+outdir+"\n")
          FileManager.deleteRecursive(outdir)
                            }

        (succs, fails)

        //val worker = new Worker
        //worker.runTests(kind, kindFiles)
      } else {
        NestUI.failure("test dir empty")
        (0, 0)
      }
    } else (0, 0)
  }

  /**
   * @return (success count, failure count)
   */
  def testCheckAll(): (Int, Int) = {
    if (runAll) { // run all tests
      posCheck = true
      negCheck = true
      runCheck = true
      jvmCheck = true
      jvm5Check = true
      //resCheck = true
    }
    val results = List(runTests("pos", posCheck, "Testing compiler (on files whose compilation should succeed)"),
                       runTests("neg", negCheck, "Testing compiler (on files whose compilation should fail)"),
                       runTests("run", runCheck, "Testing JVM backend"),
                       runTests("jvm", jvmCheck, "Testing JVM backend"),
                       runTests("jvm5", jvm5Check, "Testing JVM backend"),
                       runTests("res", resCheck, "Testing resident compiler"))
    results reduceLeft { (p: (Int, Int), q: (Int, Int)) =>
      (p._1+q._1, p._2+q._2) }
  }
}
