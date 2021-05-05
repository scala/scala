case class WhyNot(`^$#`: Int)
object Test extends App {
  val wn = WhyNot(1)
  println(wn.`^$#`)
  val WhyNot(i) = wn
  println(i)
  println(WhyNot.unapply(wn))
}
