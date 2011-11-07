trait A
trait B extends A { val x = println("early") } 
object Test extends Application {
  new B {}
}
