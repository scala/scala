/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{File, FileInputStream, FileOutputStream, PrintStream}
import java.net.URL

import scala.tools.nsc.ObjectRunner

import FileManager._

class Worker {

  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  def execTest(outDir: File, logFile: File) {
    val cmd =
      JAVACMD+
      " -classpath "+outDir+PATH_SEP+CLASSPATH+PATH_SEP+EXT_CLASSPATH+
      " -Djava.library.path="+logFile.getParentFile.getAbsolutePath+
      " -Dscalatest.output="+outDir.getAbsolutePath+
      " -Dscalatest.lib="+LATEST_LIB+
      " -Djavacmd="+JAVACMD+
      " scala.tools.nsc.MainGenericRunner"+
      " Test jvm"
    NestUI.verbose(cmd)
    val execution = Runtime.getRuntime.exec(cmd)
    //TODO: use buffered I/O
    val in = execution.getInputStream
    val out = new FileOutputStream(logFile)
    var c = in.read
    while (c != -1) {
      out.write(c)
      c = in.read
    }
    in.close
    out.close
  }

  def compareOutput(dir: File, fileBase: String, kind: String, logFile: File): Boolean = {
    // if check file exists, compare with log file
    val checkFile = {
      val chkFile = new File(dir, fileBase + ".check")
      if (chkFile.isFile)
        chkFile
      else
        new File(dir, fileBase + "-" + kind + ".check")
    }
    if (!checkFile.exists || !checkFile.canRead)
      true
    else {
      var success, equalNow = true
      val bufferSize = 1024
      val originBuffer, destBuffer = new Array[Byte](bufferSize)
      val originStream = new FileInputStream(logFile)
      val destStream = new FileInputStream(checkFile)

      var originSize = originStream.read(originBuffer)
      while (originSize >= 0) {
        if (originSize == destStream.read(destBuffer)) {
          for (idx <- 0 until originSize)
            equalNow = equalNow && (originBuffer(idx) == destBuffer(idx))
          if (!equalNow) {
            success = false
            NestUI.verbose("Diff1: diffs found")
          }
        } else {
          success = false
          NestUI.verbose("Diff1: diffs found")
        }
        originSize = originStream.read(originBuffer)
      }
      if (destStream.read(destBuffer) >= 0)
        success = false
      success
    }
  }

  def runJvmTests(kind: String, files: List[File]) {
    val compileMgr = new CompileManager
    var errors = 0
    for (file <- files) {
      var success = true
      if (!compileMgr.shouldCompile(file, kind)) {
        NestUI.failure("compilation of "+file+" failed\n")
        success = false
      } else {
        // -------- run test --------
        val fileBase: String = basename(file.getName)
        NestUI.verbose(this+" running test "+fileBase)
        val dir = file.getParentFile
        val dirpath = dir.getAbsolutePath
        val outDir = new File(dir, fileBase + "-" + kind + ".obj")
        val logFile = new File(dir, fileBase + "-" + kind + ".log")
        //TODO: detect whether we have to use Runtime.exec
        val useRuntime = true

        if (useRuntime)
          execTest(outDir, logFile)
        else {
          val classpath: List[URL] =
            outDir.toURL ::
            List(file.getParentFile.toURL) :::
            (List.fromString(CLASSPATH, PATH_SEP) map { x =>
              (new File(x)).toURL }) :::
            (List.fromString(EXT_CLASSPATH, PATH_SEP) map { x =>
              (new File(x)).toURL })
          try {
            NestUI.verbose("classpath: "+classpath)
            val out = new FileOutputStream(logFile, true)
            Console.withOut(new PrintStream(out)) {
              ObjectRunner.run(classpath, "Test", List("jvm"))
            }
            out.flush
            out.close
          } catch {
            case e: Exception =>
              NestUI.warning(e+" ("+file.getPath+")")
          }
        }
        NestUI.verbose(this+" finished running "+fileBase)

        if (!compareOutput(dir, fileBase, kind, logFile)) {
          NestUI.failure("output differs from log file\n")
          success = false
        }
      } // successful compile
      if (!success) {
        errors += 1
        NestUI.verbose("incremented errors: "+errors)
      }
    } // for each file
    NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
    NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")
  }

  def runTests(kind: String, files: List[File]): Unit = kind match {
    case "pos" =>
      val compileMgr = new CompileManager
      var errors = 0
      for (file <- files) {
        if (!compileMgr.shouldCompile(file, kind))
          errors += 1
      }
      NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
      NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")

    case "neg" =>
      val compileMgr = new CompileManager
      var errors = 0
      for (file <- files) {
        if (!compileMgr.shouldFailCompile(file, kind))
          errors += 1
      }
      NestUI.verbose("finished testing "+kind+" with "+errors+" errors")
      NestUI.verbose("created "+compileMgr.numSeparateCompilers+" separate compilers")

    case "run" =>
      runJvmTests(kind, files)

    case "jvm" =>
      runJvmTests(kind, files)
  }
}
