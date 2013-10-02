trait Super[+A]
case class Sub[B](f: B => B) extends Super[B]

object Test extends App {
  val s1 = Sub((x: Int) => x)

  (s1: Super[Any]) match { case Sub(f) => f("abc") }
}
// java.lang.ClassCastException: java.lang.String cannot be cast to java.lang.Integer
//   at scala.runtime.BoxesRunTime.unboxToInt(BoxesRunTime.java:105)
//   at Test$$anonfun$1.apply(a.scala:5)
//   at Test$.delayedEndpoint$Test$1(a.scala:7)
