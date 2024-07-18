//> using options -Xlint:-nonlocal-return
object Test {
  def wrap[K](body: => K): K = body

  def f(): Option[Int] = {
    wrap({ return Some(1) ; None })
  }

  def main(args: Array[String]): Unit = assert(f() == Some(1))
}
// java.lang.ClassCastException: scala.Some cannot be cast to scala.None$
//  at Test$$anonfun$f$1.apply(nonlocalreturn.scala:5)
//  at Test$$anonfun$f$1.apply(nonlocalreturn.scala:5)
//  at Test$.wrap(nonlocalreturn.scala:2)
