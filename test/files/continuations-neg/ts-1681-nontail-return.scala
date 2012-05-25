import scala.util.continuations._

class ReturnRepro { 
  def s1: Int @cpsParam[Any, Unit] = shift { k => k(5) } 
  def caller = reset { println(p(3)) }

  def p(i: Int): Int @cpsParam[Unit, Any] = { 
    val v= s1 + 3 
    if (v == 8)
      return v
    v + 1
  } 
}

object Test extends App {
  val repro = new ReturnRepro
  repro.caller
}
