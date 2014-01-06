object Test extends App {
 ("foo": Any) match {
   case x: Int => println("FAILED")
   case x: String => println("OK "+ x)
   case x: String => println("FAILED")
 }
}