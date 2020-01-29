
import scala.util.control.Breaks._

class Parent(val i: Int) {
  def this(p: Parent) = this(p.i)
  if (i > 40) throw new IllegalArgumentException
}
class Child extends Parent({
  val p = try new Parent(42) catch { case _: IllegalArgumentException => break() }
  p
})

object Test extends App {
  breakable(new Child)
}

/* Was (until 2.12.0-M5):
java.lang.VerifyError: Bad type on operand stack
Exception Details:
  Location:
    Child.<init>()V @2: invokespecial
  Reason:
    Type uninitializedThis (current frame, stack[1]) is not assignable to 'Child'
  Current Frame:
    bci: @2
    flags: { flagThisUninit }
    locals: { uninitializedThis }
    stack: { uninitializedThis, uninitializedThis }
  Bytecode:
    0x0000000: 2a2a b700 194c 2bb7 001c b1            

	at Main$.<init>(t8733.scala:5)
	at Main$.<clinit>(t8733.scala)
	at Main.main(t8733.scala)
*/
