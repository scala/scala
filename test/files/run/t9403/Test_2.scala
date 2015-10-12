import p.C
import scala.tools.asm.Opcodes
import scala.tools.partest.BytecodeTest
import scala.tools.partest.ASMConverters._


object Test extends BytecodeTest {
  def foo(c: C, x: Int) = c.f(x)
  def goo(c: C, x: Int) = c.g(x)

  def has(i: Instruction, c: String, m: String) = {
    val cls = loadClassNode(c)
    val mth = convertMethod(getMethod(cls, m))
    assert(mth.instructions.contains(i))
  }

  def show(): Unit = {
    assert(foo(new C, -2) == -5L)
    assert(goo(new C, -2) == -10L)

    val bipush2 = IntOp(Opcodes.BIPUSH, -2)
    has(bipush2, "p.C", "f")
    has(bipush2, "Test$", "foo")

    val sipush300 = IntOp(Opcodes.SIPUSH, -300)
    has(sipush300, "p.C", "g")
    has(sipush300, "Test$", "goo")
  }
}
