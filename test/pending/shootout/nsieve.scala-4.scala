/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/


object nsieve { 

   def nsieve(m: Int, isPrime: Array[Boolean]) = {
      for (i <- List.range(2, m)) isPrime(i) = true
      var count = 0

      for (i <- List.range(2, m)){
         if (isPrime(i)){
            var k = i+i
            while (k < m){ isPrime(k) = false; k = k+i }
            count = count + 1
         }
      }
      count
   }


   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0))
      val m = (1<<n)*10000
      val flags = new Array[Boolean](m+1)

      def printPrimes(m: Int) = {

         def pad(i: Int, width: Int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s 
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m,flags),9))
      }


      printPrimes(m)
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
