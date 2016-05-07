import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.{ ILoop, replProps }
import scala.tools.partest._

object Test extends StoreReporterDirectTest {
  def code = ???

  lazy val headerLength = replProps.welcome.lines.size
  lazy val promptLength = replProps.prompt.lines.size - 1  // extra newlines

  def compileCode(code: String, jarFileName: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", s"${testOutput.path}/$jarFileName"))(code)
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

  def app6 = """
    package test6
    class A extends Test { println("created test6.A") }
    class Z extends Test { println("created test6.Z") }
    trait Test"""

  def test1(): Unit = {
    val jar = "test1.jar"
    compileCode(app1, jar)

    val codeToRun = s"""
      |:require ${testOutput.path}/$jar
      |test.Test.test()
      |""".stripMargin.trim
    val output = ILoop.run(codeToRun, settings)
    var lines  = output.lines.drop(headerLength)
    lines      = lines drop promptLength
    val added  = lines.next
    assert (
      added.contains("Added") && added.contains("test1.jar"),
      s"[${added}] in [${output.lines.mkString("/")}]"
    )
    lines      = lines drop promptLength
    val r = lines.next
    assert(r.contains("testing..."), r)
  }

  def test2(): Unit = {
    // should reject jars with conflicting entries
    val jar1 = "test1.jar"
    val jar2 = "test2.jar"
    compileCode(app2, jar2)

    val codeToRun = s"""
      |:require ${testOutput.path}/$jar1
      |:require ${testOutput.path}/$jar2
      |""".stripMargin.trim
    val output = ILoop.run(codeToRun, settings)
    var lines  = output.lines.drop(headerLength)
    lines      = lines drop promptLength
    val added  = lines.next
    assert(added.contains("Added") && added.contains("test1.jar"), added)
    lines      = lines drop promptLength
    val msg    = lines.next
    assert(msg.contains("test2.jar") && msg.contains("contains a classfile that already exists on the classpath: test.Test$"), msg)
  }

  def test3(): Unit = {
    // should accept jars with overlapping packages, but no conflicts
    val jar1 = "test1.jar"
    val jar3 = "test3.jar"
    compileCode(app3, jar3)

    val codeToRun = s"""
      |:require ${testOutput.path}/$jar1
      |:require ${testOutput.path}/$jar3
      |test.Test3.test()
      |""".stripMargin.trim
    val output = ILoop.run(codeToRun, settings)
    var lines  = output.lines.drop(headerLength)
    lines      = lines drop promptLength
    val added  = lines.next
    assert(added.contains("Added") && added.contains("test1.jar"), added)
    lines      = lines drop (2 * promptLength + 1)
    val r = lines.next
    assert(r.contains("new object in existing package"), r)
  }

  def test4(): Unit = {
    // twice the same jar should be rejected
    val jar1   = "test1.jar"
    val codeToRun = s"""
      |:require ${testOutput.path}/$jar1
      |:require ${testOutput.path}/$jar1
      |""".stripMargin.trim
    val output = ILoop.run(codeToRun, settings)
    var lines  = output.lines.drop(headerLength)
    lines      = lines drop promptLength
    val added  = lines.next
    assert(added.contains("Added") && added.contains("test1.jar"), added)
    lines      = lines drop promptLength
    val msg    = lines.next
    assert(msg.contains("test1.jar") && msg.contains("contains a classfile that already exists on the classpath: test.Test$"), msg)
  }

  def test5(): Unit = {
    val codeToRun = ":require /does/not/exist.jar"
    val output = ILoop.run(codeToRun, settings)
    assert(!output.contains("NullPointerException"), output)
    assert(output.contains("Cannot load '/does/not/exist.jar'"), output)
  }

  def test6(): Unit = {
    // Avoid java.lang.NoClassDefFoundError triggered by the old approach of using a Java
    // classloader to parse .class files in order to read their names.
    val jar = "test6.jar"
    compileCode(app6, jar)
    val codeToRun = s"""
      |:require ${testOutput.path}/$jar
      |import test6._; new A; new Z
      |""".stripMargin.trim
    val output = ILoop.run(codeToRun, settings)
    assert(output.contains("created test6.A"), output)
    assert(output.contains("created test6.Z"), output)
  }

  def show(): Unit = {
    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
  }
}
