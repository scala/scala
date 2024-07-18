
//> using options -Xsource:3

object Test extends App {
  def f(s: String) = println(s)
  val welcomer = new Welcomer
  welcomer.greeter.greeting.foreach(f)
}
