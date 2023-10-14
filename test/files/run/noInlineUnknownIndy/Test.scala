import java.io.File

import scala.jdk.CollectionConverters._
import scala.tools.asm.tree.{ClassNode, InvokeDynamicInsnNode}
import scala.tools.asm.{Handle, Opcodes}
import scala.tools.partest.BytecodeTest.modifyClassFile
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  override def code = "class T { def foo = A_1.test }"

  override def extraSettings = {
    val classpath = List(sys.props("partest.lib"), testOutput.path).mkString(sys.props("path.separator"))
    s"-cp $classpath -opt:inline:** -Yopt-inline-heuristics:everything -Wopt:_"
  }

  override def show(): Unit = {
    val unknownBootstrapMethod = new Handle(
      Opcodes.H_INVOKESTATIC,
      "not/java/lang/SomeLambdaMetafactory",
      "notAMetaFactoryMethod",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",
      /* itf = */ false)
    modifyClassFile(new File(testOutput.jfile, "A_1.class"))((cn: ClassNode) => {
      val testMethod = cn.methods.iterator.asScala.find(_.name == "test").get
      val indy = testMethod.instructions.iterator.asScala.collect({ case i: InvokeDynamicInsnNode => i }).next()
      indy.bsm = unknownBootstrapMethod
      cn
    })

    compile()
  }
}
