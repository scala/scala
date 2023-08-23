
trait T { lazy val x: String = "hello, world" }
abstract class Abbie extends T { override def x: Any }
class K extends Abbie {
  override lazy val x: String = "goodbye, cruel world"
}
object Test extends App {
  println(new K().x)
}
