object Test extends App {
 (List(1, 2, 3): @unchecked) match {
   case Nil => println("FAIL")
   case x :: y :: xs if xs.length == 2 => println("FAIL")
   case x :: y :: xs if xs.length == 1 => println("OK "+ y)
 }
}
