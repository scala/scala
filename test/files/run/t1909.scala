// Until #1909 is fixed, if this compiles the bytecode
// will trigger a VerifyError.  This liftings and the one
// in 1909b.scala actually happen in two different places
// (uncurry and lambdalifter.)
class Ticket1909 {
  def this(value: Int) = this()
  def this(p: String) = this(try 0)
}

object Test extends App {
  new Ticket1909("")
}
