import java.io.File

import scala.jdk.CollectionConverters._
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.ClassNode
import scala.tools.partest.BytecodeTest.modifyClassFile
import scala.tools.partest.DirectTest

// The lambda in p.A_1.mkMarked has the package-private marker interface p.PkgMarker, which is
// passed to LambdaMetafactory.altMetafactory as a class constant. The method cannot be inlined
// into T, even though the lambda's implementation method p.A_1.pub is public.
//
// javac also emits a CHECKCAST to p.PkgMarker, which on its own prevents inlining. The test
// removes it, so that only the bootstrap argument is left.
object Test extends DirectTest {
  override def code = "class T { def t: Object = p.A_1.mkMarked() }"

  override def extraSettings = {
    val classpath = pathOf(sys.props("partest.lib"), testOutput.path)
    s"-cp $classpath -opt:inline:p.** -Yopt-inline-heuristics:everything"
  }

  override def show(): Unit = {
    modifyClassFile(new File(testOutput.jfile, "p/A_1.class"))((cn: ClassNode) => {
      val m = cn.methods.iterator.asScala.find(_.name == "mkMarked").get
      m.instructions.toArray.filter(_.getOpcode == Opcodes.CHECKCAST).foreach(m.instructions.remove)
      cn
    })

    compile()

    val t = Class.forName("T").getConstructor().newInstance()
    val f = t.getClass.getMethod("t").invoke(t).asInstanceOf[java.util.function.IntSupplier]
    println(f.getAsInt)
  }
}
