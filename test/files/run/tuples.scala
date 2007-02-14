import Function._

object Test extends Application {
  var xyz: (int, String, boolean) = _
  xyz = (1, "abc", true)
  Console.println(xyz)
  xyz match {
    case (1, "abc", true) => Console.println("OK")
  }
   def func(x : int, y : String, z : double) : unit = {
       Console.println("x = " + x + "; y = " + y + "; z = " + z);
   }

   def params = (2, "xxx", 3.14159)  // (*****)

   tupled(&func)(params) // call the function with all the params at once
   func(2, "xxx", 3.14159) // the same call
   (&func).apply(2, "xxx", 3.14159) // the same call

   // Composing a tuple
   def t = (1, "Hello", false)

   // Decomposing a tuple
   val (i, s, b) = t

   // all the assertions are passed
   assert(i == 1)
   assert(s == "Hello")
   assert(b == false)
}
