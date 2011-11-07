/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object sieve {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      val start = 2;
      val stop = 8192;
      val isPrime = new Array[Boolean](stop+1);
      var count: Int = 0;

      while (n>0) { 
         count = 0;

         for (i <- Iterator.range(start,stop+1)) 
            isPrime(i)=true;

         for (i <- Iterator.range(start,stop+1)) {
            if( isPrime(i) ) {
               var k = i+i;
               while (k<=stop) { isPrime(k)=false; k=k+i; }
               count = count+1;
            }
         }
         n=n-1; 
      }

      Console.println("Count: " + count);
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}



