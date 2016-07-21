import java.io.File

import scala.collection.JavaConverters._
import scala.tools.asm.tree.{ClassNode, InvokeDynamicInsnNode}
import scala.tools.asm.{Handle, Opcodes}
import scala.tools.partest.BytecodeTest.modifyClassFile
import scala.tools.partest._

object Test extends DirectTest {
  def code = ???

  def compileCode(code: String) = {
    val classpath = List(sys.props("partest.lib"), testOutput.path) mkString sys.props("path.separator")
    compileString(newCompiler("-cp", classpath, "-d", testOutput.path, "-opt:l:classpath", "-Yopt-inline-heuristics:everything", "-opt-warnings:_"))(code)
  }

  def show(): Unit = {
    val unknownBootstrapMethod = new Handle(
      Opcodes.H_INVOKESTATIC,
      "not/java/lang/SomeLambdaMetafactory",
      "notAMetaFactoryMethod",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",
      /* itf = */ false)
    modifyClassFile(new File(testOutput.toFile, "A_1.class"))((cn: ClassNode) => {
      val testMethod = cn.methods.iterator.asScala.find(_.name == "test").head
      val indy = testMethod.instructions.iterator.asScala.collect({ case i: InvokeDynamicInsnNode => i }).next()
      indy.bsm = unknownBootstrapMethod
      cn
    })

    compileCode("class T { def foo = A_1.test }")
  }
}
