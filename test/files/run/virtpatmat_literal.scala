object Test extends App {
 val a = 1
 1 match {
   case 2 => println("FAILED")
   case 1 => println("OK")
   case `a` => println("FAILED")
 }

 val one = 1
 1 match {
   case 2 => println("FAILED")
   case `one` => println("OK")
   case 1 => println("FAILED")
 }

 1 match {
   case 2 => println("FAILED")
   case Test.one => println("OK")
   case 1 => println("FAILED")
 }

}