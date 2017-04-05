import scala.tools.partest._
import java.io.File

object Test extends DirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }
  // TODO
  // Don't assume output is on physical disk
  // Let the compiler tell us output dir
  // val sc = newCompiler("-cp", classpath, "-d", testOutput.path)
  // val out = sc.settings.outputDirs.getSingleOutput.get
  def show(): Unit = {
    // Don't crash when we find a file 'a' where package 'a' should go.
    scala.reflect.io.File(testOutput.path + "/a").writeAll("a")
    compileCode("package a { class B }")
    //and the same in th case of a subdirectory
    compileCode("package a.b { class B }")

    // Don't crash when we find a directory 'a' where class 'a' should go.
    // or maybe more realistically we dont crash if for some reason we cant write the file
    val blocking = scala.reflect.io.File(testOutput) / "a.class" / "b" / "c"
    blocking.mkdirs()
    blocking.createFile()
    compileCode("class a")
  }
}
