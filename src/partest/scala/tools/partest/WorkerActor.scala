/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.partest

import java.io.{BufferedInputStream, BufferedReader, File, FileReader, FileWriter,
                FileInputStream, FileOutputStream, InputStreamReader,
                PrintStream, PrintWriter}
import java.net.URL

import scala.actors.Actor
import scala.tools.nsc.{Global, ObjectRunner, Settings}
import scala.tools.nsc.reporters.{Reporter, AbstractReporter, ConsoleReporter}


class ExtConsoleReporter(override val settings: Settings, reader: BufferedReader, var writer: PrintWriter) extends ConsoleReporter(settings, reader, writer) {
  def this(settings: Settings) = {
    this(settings, Console.in, new PrintWriter(new FileWriter("/dev/null")))
  }
  def hasWarnings: Boolean = WARNING.count != 0
}

class ExtGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  override def inform(msg: String) {}
}

/**
 * @author  Adriaan Moors, Thomas Hofer
 * @version 1.0
 */
class WorkerActor(val master: MasterActor, val settings: Settings, var reporter: ExtConsoleReporter) extends Actor {
  import scala.actors.Actor._

  def newGlobal: ExtGlobal = new ExtGlobal(settings, reporter)

  def newGlobal(log: File): ExtGlobal = {
    reporter = new ExtConsoleReporter(new Settings(x => ()), Console.in, new PrintWriter(new FileWriter(log)))
    reporter.shortname = true
    newGlobal
  }

  private def dirDelete(dir: File) {
    if (dir.isDirectory) {
      for (file <- dir.list) dirDelete(new File(dir, file))
    }
    dir.delete
  }

  private val PATH_SEP = java.io.File.pathSeparatorChar
  private val CLASSPATH = System.getProperty("CLASSPATH", "")
  private val EXT_CLASSPATH = System.getProperty("JVMEXTCP", "")

  def act() {
    var compiler = newGlobal
    val bufferSize = 1024
    val originBuffer = new Array[Byte](bufferSize)
    val destBuffer = new Array[Byte](bufferSize)
    loop {
      react {
        case (test: Test, bypass: Boolean, conservative: Boolean) => {
          var bypassObjectRunner = bypass
          if (!bypassObjectRunner) {
            // TODO check the shootout source files for "dangerous" patterns, such as: Console.read, Scheduler.impl,
            val dangerousCode = List("Console.read", "Scheduler.impl", "System.exit", "System.out").foldLeft("")((x, y) => x + " -e " + y)
            val grepCmd = "grep " + test.file.getPath + " " + dangerousCode
            TestRunner.printVerbose("grep cmd: " + grepCmd + "\n")

            val grep = Runtime.getRuntime.exec(grepCmd)

            val in = new BufferedReader(new InputStreamReader(grep.getInputStream))

            val line = in.readLine
            bypassObjectRunner = (line != null)

            in.close

            //println(bypassObjectRunner)
          }

          var start = System.currentTimeMillis
          //println("Starting..." + test.kind + " " + test.test)

          var toCompile = List(test.file.getPath)

          var outDir: File = new File(test.dir, test.fileBase + "-" + test.kind + ".obj")
          if (! outDir.exists) {
            outDir.mkdir
            //println(this.toString + " " + "Created " + outDir)
          } else {
            //println(this.toString + " " + "Didn't need to create " + outDir)
          }
          test match {
            case NegTest(_) =>
              compiler = newGlobal(test.logFile)

            case JVMTest(_) =>
              //println(test.file.getPath + ": " + test.checkFile.exists + " / " + test.logFile.exists)
              if (test.checkFile.exists) {
                var checkReader = new BufferedReader(new FileReader(test.checkFile))
                var firstLine = checkReader.readLine
                if (firstLine != null && firstLine.startsWith("warning")) {
                  //reporter = new ExtConsoleReporter(new Settings(x => ()), Console.in, new PrintWriter(logFile))
                  //reporter.shortname = true
                  compiler = newGlobal(test.logFile)
                }
              } else if (conservative) compiler = newGlobal

            case ShootoutTest(_) =>
              var testFile = new File(outDir, "Test.scala")
              if (testFile.exists) {
                toCompile = List(testFile.toString)
                //println(this.toString + " ready to compile :" + toCompile)
              } else {
                // BASH script couldn't create test file...
              }
              if (test.checkFile.exists) {
                var checkReader = new BufferedReader(new FileReader(test.checkFile))
                var firstLine = checkReader.readLine
                if (firstLine.startsWith("warning")) {
                  //reporter = new ExtConsoleReporter(new Settings(x => ()), Console.in, new PrintWriter(logFile))
                  //reporter.shortname = true
                  compiler = newGlobal(test.logFile)
                }
              } else if (conservative) compiler = newGlobal

            case _ =>
          }

          val c = compiler

          //println("about to define compilation settings...")

          test.defineSettings(settings)
          try {
            //println(this.toString + " " + "Launching compiler on " + toCompile)
            (new c.Run) compile toCompile
            reporter.printSummary
            reporter.writer.flush
            reporter.writer.close
            //println(this.toString + " " + "Finished compiling " + test.fileBase)
          } catch {
            case e => {
              e.printStackTrace
              reporter.error(null, "IO/Error")
            }
          }
          (reporter.hasErrors, test) match {
            case (_, NegTest(_)) =>
            case (true, _) =>
              compiler = newGlobal
              val c = compiler
              try {
                (new c.Run) compile toCompile
              } catch {
                case e => reporter.error(null, "IO/Error")
              }
            case _ =>
          }

          var result = test match {
            case NegTest(_) => reporter.hasErrors
            case _ => !reporter.hasErrors
          }

          (bypassObjectRunner, result, test) match {
            case (_, _, PosTest(_)) =>
            case (_, _, NegTest(_)) =>
            case (false, true, _) =>
              System.setProperty("scalatest.output", outDir.toString)
              test match {
                case ShootoutTest(_) => System.setProperty("scalatest.cwd", test.dir)
                case _ => {}
              }

              var classpath: List[URL] =
                outDir.toURL ::
                List((new File(test.dir)).toURL) :::
                (List.fromString(CLASSPATH, PATH_SEP) map { x => (new File(x)).toURL }) :::
                (List.fromString(EXT_CLASSPATH, PATH_SEP) map { x => (new File(x)).toURL })

              try {
                //println(this.toString + " " + "Launching test " + test.fileBase)
                var out = new FileOutputStream(test.logFile, true)
                Console.withOut(new PrintStream(out)) {
                  ObjectRunner.run(classpath, "Test", List("jvm"))
                }
                out.flush
                out.close
                //println(this.toString + " " + "Finished running " + test.fileBase)
              } catch { case t => println(t) }

            case _ =>
          }
          (!bypassObjectRunner && result, test.checkFile.exists, test) match {
            case (_, _, PosTest(_)) =>
            case (true, true, _) =>
              /*var cmd: String = "diff " + test.logFile + "  " + test.checkFile
              //println(this.toString + " Comparing files " + test.fileBase)
              var proc: Process = Runtime.getRuntime.exec(cmd)
              proc.waitFor
              result = (proc.exitValue == 0)*/
              var equalNow = true
              if (test.checkFile.canRead) {
                val originStream = new FileInputStream(test.logFile)
                val destStream = new FileInputStream(test.checkFile)
                var originSize = originStream.read(originBuffer)
                while (originSize >= 0) {
                  if (originSize == destStream.read(destBuffer)) {
                    for (idx <- 0 until originSize)
                      equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))
                    if (!equalNow) {
                      result = false
                      //println("Diff1: diffs found")
                    }
                  }
                  else {
                    result = false
                    //println("Diff1: diffs found")
                  }
                  originSize = originStream.read(originBuffer)
                }
                if (destStream.read(destBuffer) >= 0) result = false
              }

            case _ =>
              //println("Not testing diff... " + test.test)
          }

          (bypassObjectRunner || !result, test) match {
            case (_, PosTest(_)) =>
            case (_, NegTest(_)) =>
            case (true, _) =>
              result = true
              //var javaoptsFile = new File(test.dir, test.fileBase + ".javaopts")
              //var javaNewOpts = (new BufferedFileReader(javaoptsFile)).readLine
              //if (javaoptsFile.exists && javaNewOpts != null) {}
              //Use Runtime.exec to execute the compiled file and pipe the standard system
              //out and the console out to the logfile
              var cmd =
                "env JAVACMD=java JAVA_OPTS=-Djava.library.path=\"" + test.dir + "\" " +
                System.getProperty("SCALA")+
                " -Dscalatest.lib=\"" + System.getProperty("scalatest.lib") + "\" " +
                "-Dscalatest.cwd=\"" + test.dir + "\" " +
                "-Dscalatest.output=" + outDir +
                " -classpath " + outDir + PATH_SEP + CLASSPATH + PATH_SEP + EXT_CLASSPATH +
                " Test jvm"

              TestRunner.printVerbose("Worker command: " + cmd)

              var execution = Runtime.getRuntime.exec(cmd)

              var in = execution.getInputStream
              var out = new FileOutputStream(test.logFile)

              var c = in.read
              while (c != -1) {
                out.write(c)
                c = in.read
              }

              in.close
              out.close

              //println("Running diff")

              /*var diff = Runtime.getRuntime.exec("diff " + test.logFile + " " + test.checkFile)
              diff.waitFor

              result = (diff.exitValue == 0)*/
              var equalNow = true
              if (test.checkFile.canRead) {
                val originStream = new FileInputStream(test.logFile)
                val destStream = new FileInputStream(test.checkFile)
                var originSize = originStream.read(originBuffer)
                while (originSize >= 0) {
                  if (originSize == destStream.read(destBuffer)) {
                    for (idx <- 0 until originSize)
                      equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))
                    if (!equalNow) {
                      result = false
                      //println("Differences found between the log and check files..")
                    }
                  }
                  else {
                    result = false
                    //println("Differences found between the log and check files..")
                  }

                  originSize = originStream.read(originBuffer)
                }
                if (destStream.read(destBuffer) >= 0) result = false
              }
              //else reportMissing(originFile)

            case _ =>
              //println("Not Using runtime... " + test.test)
          }
          test.logFile.delete

          var end = System.currentTimeMillis

          //println(test.test + ": " + (end - start))

          //printSuccess(this.toString + " " + fileBase + ": "+ result + "\n")
          master ! (test.kind, result, test.file)
          dirDelete(outDir)
        }
        case false =>
          exit

        case msg =>
          println("Unknown message : " + msg)
      }
    }
  }
}
