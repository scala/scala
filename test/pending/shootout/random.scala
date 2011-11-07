/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object random {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      var result: Double = 0

      while (n>0) { result=generate(100.0); n=n-1; }

      Console.printf("{0,number,#.000000000}\n", result)
   }

   private val IM = 139968;
   private val IA = 3877;
   private val IC = 29573;
   private var seed = 42;

   def generate(max: Double) = {
      seed = (seed * IA + IC) % IM;
      max * seed / IM;
   }

   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}
