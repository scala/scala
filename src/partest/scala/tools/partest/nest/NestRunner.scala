/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter}

import scala.actors.Actor._

object NestRunner {
  private val version = System.getProperty("java.version", "")
  private val isJava5 = version matches "1.[5|6|7].*"

  private val numActors = Integer.parseInt(System.getProperty("scalatest.actors", "8"))

  private var posCheck = false
  private var negCheck = false
  private var jvmCheck = false
  private var jvm5Check = false
  private var runCheck = false
  private var shootoutCheck = false

  private var conservative = false

  var showDiff = false
  var showLog = false
  var failed = false

  private var testFiles: List[File] = List()
  private val con = new PrintStream(Console.out)
  private var out = con

  private val errors =
    Integer.parseInt(System.getProperty("scalatest.errors", "0"))

  def main(args: Array[String]) {
    NestUI.initialize(NestUI.MANY)

    if (args.length == 0)
      NestUI.usage()
    else {
      for (arg <- args) {
        arg match {
          case "--pos"          => posCheck = true
          case "--neg"          => negCheck = true
          case "--jvm"          => jvmCheck = true
          case "--jvm5"         => jvm5Check = true
          case "--run"          => runCheck = true
          case "--shootout"     => shootoutCheck = true
          case "--conservative" => conservative = true
          case "--verbose"      => NestUI._verbose = true
          case "--show-diff"    => showDiff = true
          case "--show-log"     => showLog = true
          case "--failed"       => failed = true
          case "--version"      => //todo: printVersion
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
      val writer = new StringWriter
      val errWriter = new StringWriter
      val appender = new StreamAppender(new InputStreamReader(in), writer)
      val errApp = new StreamAppender(new InputStreamReader(err), errWriter)
      appender.start()
      errApp.start()
      val exitCode = proc.waitFor()
      NestUI.verbose("exit code: "+exitCode)
      appender.join()
      errApp.join()
      val scalaVersion = writer.toString + errWriter.toString

      NestUI.outline("Scala version is    : "+scalaVersion)
      NestUI.outline("Scalac options are  : "+FileManager.SCALAC_OPTS+"\n")

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
        else fileMgr.getFiles(kind, check)
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
        workers foreach { w =>
          receive {
            case Results(s, f) => succs += s; fails += f
          }
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
    val results = List(runTests("pos", posCheck, "Testing compiler (on files whose compilation should succeed)"),
                       runTests("run", runCheck, "Testing JVM backend"),
                       runTests("jvm", jvmCheck, "Testing JVM backend"),
                       runTests("jvm5", jvm5Check, "Testing JVM backend"))
    results reduceLeft { (p: (Int, Int), q: (Int, Int)) =>
      (p._1+q._1, p._2+q._2) }
  }
}
