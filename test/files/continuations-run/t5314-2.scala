import scala.util.continuations._

class ReturnRepro {
  def s1: Int @cps[Any] = shift { k => k(5) }
  def caller = reset { println(p(3)) }
  def caller2 = reset { println(p2(3)) }
  def caller3 = reset { println(p3(3)) }

  def p(i: Int): Int @cps[Any] = {
    val v= s1 + 3
    return v
  }

  def p2(i: Int): Int @cps[Any] = {
    val v = s1 + 3
    if (v > 0) {
      println("hi")
      return v
    } else {
      println("hi")
      return 8
    }
  }

  def p3(i: Int): Int @cps[Any] = {
    val v = s1 + 3
    try {
      println("from try")
      return v
    } catch {
      case e: Exception =>
        println("from catch")
        return 7
    }
  }

}

object Test extends App {
  val repro = new ReturnRepro
  repro.caller
  repro.caller2
  repro.caller3
}
