import scala.reflect.io.Streamable
import scala.tools.asm.{ClassWriter, ClassReader}
import scala.tools.asm.tree.ClassNode
import scala.tools.partest._
import scala.tools.partest.BytecodeTest.modifyClassFile
import java.io.{FileOutputStream, FileInputStream, File}

object Test extends DirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path))(code)
  }

  def app = """
    object O {
      new test.Annotated
    }
  """

  def show(): Unit = {
    compileCode(app)
    modifyClassFile(new File(testOutput.toFile, "test/Annotated.class")) {
      (cn: ClassNode) =>
        // As investigated https://issues.scala-lang.org/browse/SI-2464?focusedCommentId=64521&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-64521
        // classfiles in the wild sometimes lack the required InnerClass attribute for nested enums that
        // are referenced in an annotation. I don't know what compiler or bytecode processor leaves things
        // that way, but this test makes sure we don't crash.
        cn.innerClasses.clear()
        cn
    }
    compileCode(app)
  }
}
