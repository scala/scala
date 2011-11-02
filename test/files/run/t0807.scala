trait A
trait B extends A { val x = println("early") } 
object Test extends App {
  new B {}
}
