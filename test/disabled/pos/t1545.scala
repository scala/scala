// According to the spec this code should not be legal.
// Disabling for now.
object Main extends App {

    case class Foo (field : Option[String])

    val x : PartialFunction[Foo,Int] =
        {
            c => c.field match {
                case Some (s) => 42
                case None     => 99
            }
        }
        
    println (x (Foo (None))) // prints 99
    println (x (Foo (Some ("foo")))) // prints 42
        
}
