// scalac: -Xsource:3

object BrokenLit {
  trait T { lazy val x: String = "hello, world" }
  abstract class Abbie extends T { override def x: Any }
}
object Test extends App {
  import BrokenLit.*
  class K extends Abbie {
    override lazy val x: String = "goodbye, cruel world"
  }
  println(new K().x)
}
