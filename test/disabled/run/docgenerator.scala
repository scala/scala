object Test {
  import java.io.{File, FileReader, FileWriter}

  /** Tests the generation of the HTML documentation for some Scala
   *  code samples (see value 'code' below) with different scaladoc
   *  options (currently -access:<value>).
   *
   *  @author Stephane Micheloud
   */
  def main(args: Array[String]) {
    // overwrites value of UrlContext.generator in file DocUtil.scala
    System.setProperty("doc.generator", "scaladoc")
    var dirname = System.getProperty("partest.output")
    if (dirname eq null) dirname = System.getProperty("java.io.tmpdir")
    val tmpDir = new File(dirname)
    tmpDir.mkdirs()
    test1(tmpDir)
    test2(tmpDir)
  }

  private def test1(tmpDir: File) {
    def testOptions(inFile: File, outDirName: String, opts: String*) {
      val outDir = createDir(tmpDir, outDirName)
      val args = Array.concat(Array("-d", outDir.getPath, inFile.getPath), opts.toArray:Array[String])
      if (MainDoc.main0(args)) {
        for (name <- List("all-classes.html", "index.html")) {
          val outFile = new File(outDir, name)
          val n = outFile.length.toInt
          val in = new FileReader(outFile)
          val cbuf = new Array[Char](n)
          in.read(cbuf, 0, n)
          println(new String(cbuf))
        }
        println
      }
    }
    val inFile = {
      val f = new File(tmpDir.getPath, "docgenerator1.scala")
      val writer = new FileWriter(f)
      writer.write(code1, 0, code1.length)
      writer.close
      f
    }
    testOptions(inFile, "test1", "") // none (default is -access:protected)
    testOptions(inFile, "test2", "-access:public")
    testOptions(inFile, "test3", "-access:protected")
    testOptions(inFile, "test4", "-access:private")
  }

  private def test2(tmpDir: File) {
    val code ="""
package annots

@deprecated("msg")
object Foo { val x = 0 }

@deprecated("msg")
class Bar { val x = 1 }

object Foo1 {
  @deprecated("msg")
  object Foo11 { val x = 3 }
}

class Bar1 {
  @deprecated("msg")
  object Foo11 { val x = 2 }
}

class Bar2 {
  def bar {
    @deprecated("msg")
    object Foo21 { val x = 4 }
    ()
  }
}

object Foo2 {
  def foo {
    @deprecated("msg")
    object Foo21 { val x = 5 }
    ()
  }
}
"""
    val inFile = {
      val f = new File(tmpDir.getPath, "docgenerator2.scala")
      val writer = new FileWriter(f)
      writer.write(code, 0, code.length)
      writer.close
      f
    }
    val outDir = createDir(tmpDir, "annots1")
    val args = Array.concat(Array("-d", outDir.getPath, inFile.getPath))
    if (MainDoc.main0(args)) {
      for (name <- List("all-classes.html", "index.html")) {
        val outFile = new File(outDir, name)
        val n = outFile.length.toInt
        val in = new FileReader(outFile)
        val cbuf = new Array[Char](n)
        in.read(cbuf, 0, n)
        println(new String(cbuf))
      }
      println
    }
  }

  object MainDoc {
    import scala.tools.nsc._
    import scala.tools.nsc.doc.DefaultDocDriver
    import scala.tools.nsc.reporters.ConsoleReporter
    def error(msg: String) { Console.err.println(msg) }
    var reporter: ConsoleReporter = _
    def process(args: Array[String]) {
      val docSettings = new scala.tools.nsc.doc.Settings(error)
      // when running that compiler, give it a scala-library to the classpath
      docSettings.classpath.value = System.getProperty("java.class.path")
      reporter = new ConsoleReporter(docSettings)
      val command = new CompilerCommand(args.toList, docSettings)
      try {
        object compiler extends Global(command.settings, reporter) {
	  override protected def computeInternalPhases() : Unit = {
	    phasesSet += syntaxAnalyzer
	    phasesSet += analyzer.namerFactory
	    phasesSet += analyzer.typerFactory
	  }
	  override def forScaladoc = true
	}
        if (reporter.hasErrors) {
          reporter.flush()
          return
        }
        val run = new compiler.Run
        run compile command.files
        object generator extends DefaultDocDriver {
          lazy val global: compiler.type = compiler
          lazy val settings = docSettings
        }
        generator process run.units
        reporter.printSummary()
      } catch {
        case ex @ FatalError(msg) =>
          if (command.settings.debug.value)
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
    }
    def main(args: Array[String]) {
      process(args)
      exit(if (reporter.hasErrors) 1 else 0)
    }
    // main returning a status (no exit code)
    def main0(args: Array[String]): Boolean = {
      process(args)
      !reporter.hasErrors
    }
  }

  private def createDir(parent: File, dirname: String): File = {
    val outDir = new File(parent, dirname)
    outDir.mkdir
    outDir
  }

  private val code1 = """
package examples

abstract class C0 {
  def foo_public
  protected def foo_protected
  private def foo_private {}
  class C1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected class C1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private class C1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

protected abstract class C0_Protected {
  def foo_public
  protected def foo_protected
  private def foo_private {}
  class C1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected class C1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private class C1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

private abstract class C0_Private {
  def foo_public
  protected def foo_protected
  private def foo_private {}
  class C1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected class C1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private class C1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}


object obj0 {
  def bar_public {}
  protected def bar_protected {}
  private def bar_private {}
  object obj1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected object obj1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private object obj1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

protected object obj0_Protected {
  def bar_public {}
  protected def bar_protected {}
  private def bar_private {}
  object obj1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected object obj1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private object obj1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

private object obj0_Private {
  def bar_public {}
  protected def bar_protected {}
  private def bar_private {}
  object obj1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected object obj1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private object obj1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}
"""
}
