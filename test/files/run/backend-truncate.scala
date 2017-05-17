import scala.tools.partest._
import java.io.File

object Test extends DirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }
  // Ensure that when we compile a file we only contain the data from the generated code
  def show(): Unit = {
    compileCode("class a")
    val file = scala.reflect.io.File(testOutput.path + "/a.class").toFile
    val origLength = file.length
    //add sore extra data  to the file
    file.appendAll("FooBar"*1000)
    assert (origLength < file.length, s"file didn't grow")

    //assert the file is back to the correct size
    compileCode("class a")
    assert (origLength == file.length, s"expected size $origLength but found ${file.length}")
  }
}
