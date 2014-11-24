import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop
import scala.tools.nsc.settings.ClassPathImplementationType
import scala.tools.partest._

object Test extends StoreReporterDirectTest {
  def code = ???

  def compileCode(code: String, jarFileName: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", s"${testOutput.path}/$jarFileName"))(code)
  }

  // TODO flat classpath doesn't support the classpath invalidation yet so we force using the recursive one
  // it's the only test which needed such a workaround
  def sets = {
    val settings = new Settings
    settings.YclasspathImpl.value = ClassPathImplementationType.Recursive
    settings
  }

  def app1 = """
    package test

    object Test extends App {
      def test(): Unit = {
        println("testing...")
      }
    }"""

  def app2 = """
    package test

    object Test extends App {
      def test(): Unit = {
        println("testing differently...")
      }
    }"""

  def app3 = """
    package test

    object Test3 extends App {
      def test(): Unit = {
        println("new object in existing package")
      }
    }"""

  def test1(): Unit = {
    val jar = "test1.jar"
    compileCode(app1, jar)

    val codeToRun = toCodeInSeparateLines(s":require ${testOutput.path}/$jar", "test.Test.test()")
    val output = ILoop.run(codeToRun, sets)
    val lines  = output.split("\n")
    val res1   = lines(4).contains("Added") && lines(4).contains("test1.jar")
    val res2   = lines(lines.length-3).contains("testing...")

    println(s"test1 res1: $res1")
    println(s"test1 res2: $res2")
  }

  def test2(): Unit = {
    // should reject jars with conflicting entries
    val jar1 = "test1.jar"
    val jar2 = "test2.jar"
    compileCode(app2, jar2)

    val codeToRun = toCodeInSeparateLines(s":require ${testOutput.path}/$jar1", s":require ${testOutput.path}/$jar2")
    val output = ILoop.run(codeToRun, sets)
    val lines  = output.split("\n")
    val res1   = lines(4).contains("Added") && lines(4).contains("test1.jar")
    val res2   = lines(lines.length-3).contains("test2.jar") && lines(lines.length-3).contains("existing classpath entries conflict")

    println(s"test2 res1: $res1")
    println(s"test2 res2: $res2")
  }

  def test3(): Unit = {
    // should accept jars with overlapping packages, but no conflicts
    val jar1 = "test1.jar"
    val jar3 = "test3.jar"
    compileCode(app3, jar3)

    val codeToRun = toCodeInSeparateLines(s":require ${testOutput.path}/$jar1", s":require ${testOutput.path}/$jar3", "test.Test3.test()")
    val output = ILoop.run(codeToRun, sets)
    val lines  = output.split("\n")
    val res1   = lines(4).contains("Added") && lines(4).contains("test1.jar")
    val res2   = lines(lines.length-3).contains("new object in existing package")

    println(s"test3 res1: $res1")
    println(s"test3 res2: $res2")
  }

  def test4(): Unit = {
    // twice the same jar should be rejected
    val jar1   = "test1.jar"
    val codeToRun = toCodeInSeparateLines(s":require ${testOutput.path}/$jar1", s":require ${testOutput.path}/$jar1")
    val output = ILoop.run(codeToRun, sets)
    val lines  = output.split("\n")
    val res1   = lines(4).contains("Added") && lines(4).contains("test1.jar")
    val res2   = lines(lines.length-3).contains("test1.jar") && lines(lines.length-3).contains("existing classpath entries conflict")

    println(s"test4 res1: $res1")
    println(s"test4 res2: $res2")
  }

  def show(): Unit = {
    test1()
    test2()
    test3()
    test4()
  }

  def toCodeInSeparateLines(lines: String*): String = lines map (_ + "\n") mkString
}
