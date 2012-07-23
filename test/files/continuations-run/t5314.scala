import scala.util.continuations._

class ReturnRepro { 
  def s1: Int @cpsParam[Any, Unit] = shift { k => k(5) } 
  def caller = reset { println(p(3)) }
  def caller2 = reset { println(p2(3)) }

  def p(i: Int): Int @cpsParam[Unit, Any] = { 
    val v= s1 + 3 
    return v 
  } 

  def p2(i: Int): Int @cpsParam[Unit, Any] = {
    val v = s1 + 3
    if (v > 0) {
      println("hi")
      return v
    } else {
      println("hi")
      return 8
    }
  }
}

object Test extends App {
  def foo(x:Int): Int @cps[Int] = shift { k => k(x) }

  def bar(x:Int): Int @cps[Int] = return foo(x)

  def nocps(x: Int): Int = { return x; x }

  val repro = new ReturnRepro
  repro.caller
  repro.caller2

  reset {
    val res = bar(8)
    println(res)
    res
  }
}
