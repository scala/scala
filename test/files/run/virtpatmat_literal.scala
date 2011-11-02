object Test extends App {
 1 match { 
   case 2 => println("FAILED") 
   case 1 => println("OK") 
   case 1 => println("FAILED") 
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