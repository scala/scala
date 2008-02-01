/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, PrintStream, FileOutputStream}

object NestRunner {
  private val version = System.getProperty("java.version", "")
  private val isJava5 = version matches "1.[5|6|7].*"

  private var posCheck = false
  private var negCheck = false
  private var jvmCheck = false
  private var runCheck = false
  private var shootoutCheck = false

  private var conservative = false

  private var testFiles: List[File] = List()
  private val con = new PrintStream(Console.out)
  private var out = con

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
          case "--run"          => runCheck = true
          case "--shootout"     => shootoutCheck = true
          case "--conservative" => conservative = true
          case "--verbose"      => NestUI._verbose = true
          case "--version"      => //todo: printVersion
          case _ =>
            if (arg endsWith ".scala") {
              val file = new File(arg)
              if (file.isFile)
                testFiles = file :: testFiles
              else {
                NestUI.failure("File \"" + arg + "\" not found")
                exit(1)
              }
            } else if (out eq con) {
              val file = new File(arg)
              if (file.isFile || file.createNewFile)
                out = new PrintStream(new FileOutputStream(file))
              else {
                NestUI.failure("Result file \"" + arg + "\" not found")
                exit(1)
              }
            } else
              NestUI.usage()
        }
      }
      go()
    }
  }

  def runTests(kind: String, check: Boolean, msg: String) {
    if (check) {
      val fileMgr = new FileManager
      val kindFiles =
        if (!testFiles.isEmpty) testFiles
        else fileMgr.getFiles(kind, check)
      if (!kindFiles.isEmpty) {
        NestUI.outline("\n"+msg+"\n")
        val worker = new Worker
        worker.runTests(kind, kindFiles)
      }
    }
  }

  def go() {
    runTests("pos", posCheck, "Testing compiler (on files whose compilation should succeed)")
    runTests("run", runCheck, "Testing JVM backend")
    runTests("jvm", jvmCheck, "Testing JVM backend")
  }

}
