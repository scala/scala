import scala.tools.asm.tree.ClassNode
import scala.tools.partest._
import scala.tools.partest.BytecodeTest.modifyClassFile
import java.io.File

object Test extends DirectTest {
  def code = """
    object O {
      new test.Annotated
    }
  """

  override def extraSettings = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    s"-cp $classpath"
  }

  def show(): Unit = {
    compile()
    modifyClassFile(new File(testOutput.toFile, "test/Annotated.class")) {
      (cn: ClassNode) =>
        // As investigated https://github.com/scala/bug/issues/2464#issuecomment-292371985
        // classfiles in the wild sometimes lack the required InnerClass attribute for nested enums that
        // are referenced in an annotation. I don't know what compiler or bytecode processor leaves things
        // that way, but this test makes sure we don't crash.
        cn.innerClasses.clear()
        cn
    }
    compile()
  }
}
