object Foo { def apply(x: String) = new Foo(x) }
class Foo(name: String)
case object Bar extends Foo("Bar")
case class Baz() extends Foo("Baz") 
object Test extends App {
  Foo("Bar") match { 
    case Bar => println("What?") 
    case _ => println("OK")
  }
  Foo("Baz") match { 
    case Baz() => println("What?") 
    case _ => println("OK")
  }   
}
