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

  def foo2(x:Int): Int @cps[Int] = 7
  def bar2(x:Int): Int @cps[Int] = { foo2(x); return 7 }
  def bar3(x:Int): Int @cps[Int] = { foo2(x); if (x == 7) return 7 else return foo2(x) }
  def bar4(x:Int): Int @cps[Int] = { foo2(x); if (x == 7) return 7 else foo2(x) }
  def bar5(x:Int): Int @cps[Int] = { foo2(x); if (x == 7) return 7 else 8 }
  println(reset { bar2(10) })
  println(reset { bar3(10) })
  println(reset { bar4(10) })
  println(reset { bar5(10) })

  /* original test case */
  val repro = new ReturnRepro
  repro.caller
  repro.caller2

  reset {
    val res = bar(8)
    println(res)
    res
  }
}
