import scala.tools.partest.{BytecodeTest, ASMConverters}
import ASMConverters._

class D {
  // This is compiled with `A_1.class` on the classpath. When inlining `flop` (which invokes
  // `A_1.bar`), the inliner can check that the call to `A_1.bar` can be safely inlined into a
  // different classfile (D). See also comment in B_1.scala.
  def m(b: B) = b.flop
}

object Test extends BytecodeTest {
  def show: Unit = {
    val gIns = instructionsFromMethod(getMethod(loadClassNode("B"), "g"))
    val hIns = instructionsFromMethod(getMethod(loadClassNode("C"), "h"))
    for (i <- List(gIns, hIns)) {
      assert(i exists {
        // `flop` is not inlined
        case Invoke(_, _, "flop", "()I", _) => true
        case _ => false
      }, i mkString "\n")
    }

    val mIns = instructionsFromMethod(getMethod(loadClassNode("D"), "m"))
    assert(mIns exists {
      // `flop` is inlined, we get a call to `bar`
      case Invoke(_, _, "bar", "()I", _) => true
      case _ => false
    }, mIns mkString "\n")
  }
}
