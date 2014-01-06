/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import java.io._

object fasta {
   def main(args: Array[String]) = {

      val ALU =
         "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
         "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
         "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
         "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
         "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
         "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
         "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

      val _IUB = Array(
         ('a', 0.27),
         ('c', 0.12),
         ('g', 0.12),
         ('t', 0.27),

         ('B', 0.02),
         ('D', 0.02),
         ('H', 0.02),
         ('K', 0.02),
         ('M', 0.02),
         ('N', 0.02),
         ('R', 0.02),
         ('S', 0.02),
         ('V', 0.02),
         ('W', 0.02),
         ('Y', 0.02)
      )

      val IUB = makeCumulative(_IUB)

      val _HomoSapiens = Array(
         ('a', 0.3029549426680),
         ('c', 0.1979883004921),
         ('g', 0.1975473066391),
         ('t', 0.3015094502008)
      )

      val HomoSapiens = makeCumulative(_HomoSapiens)


      val n = Integer parseInt(args(0))
      val s = new FastaOutputStream(System.out)

      s.writeDescription("ONE Homo sapiens alu")
      s.writeRepeatingSequence(ALU,n*2)

      s.writeDescription("TWO IUB ambiguity codes")
      s.writeRandomSequence(IUB,n*3)

      s.writeDescription("THREE Homo sapiens frequency")
      s.writeRandomSequence(HomoSapiens,n*5)

      s.close
   }

   def makeCumulative(a: Array[Tuple2[Char,Double]]) = {
      var cp = 0.0
      a map (frequency =>
         frequency match {
            case (code,percent) =>
               cp = cp + percent; new Frequency(code.toByte,cp)
         }
      )
   }

}


// We could use instances of Pair or Tuple2 but specific labels
// make the code more readable than index numbers

class Frequency(_code: Byte, _percent: Double){
   var code = _code; var percent = _percent;
}


// extend the Java BufferedOutputStream class

class FastaOutputStream(out: OutputStream) extends BufferedOutputStream(out) {

   private val LineLength = 60
   private val nl = '\n'.toByte

   def writeDescription(desc: String) = { write( (">" + desc + "\n").getBytes ) }

   def writeRepeatingSequence(_alu: String, length: Int) = {
      val alu = _alu.getBytes
      var n = length; var k = 0; val kn = alu.length;

      while (n > 0) {
         val m = if (n < LineLength) n else LineLength

         var i = 0
         while (i < m){
            if (k == kn) k = 0
            val b = alu(k)
            if (count < buf.length){ buf(count) = b; count = count + 1 }
            else { write(b) } // flush buffer
            k = k+1
            i = i+1
         }

         write(nl)
         n = n - LineLength
      }

   }

   def writeRandomSequence(distribution: Array[Frequency], length: Int) = {
      var n = length
      while (n > 0) {
         val m = if (n < LineLength) n else LineLength

         var i = 0
         while (i < m){
            val b = selectRandom(distribution)
            if (count < buf.length){ buf(count) = b; count = count + 1 }
            else { write(b) } // flush buffer
            i = i+1
         }

         if (count < buf.length){ buf(count) = nl; count = count + 1 }
         else { write(nl) } // flush buffer
         n = n - LineLength
      }
   }

   private def selectRandom(distribution: Array[Frequency]): Byte = {
      val n = distribution.length
      val r = RandomNumber scaledTo(1.0)

      var i = 0
      while (i < n) {
         if (r < distribution(i).percent) return distribution(i).code
         i = i+1
      }
      return distribution(n-1).code
   }
}


object RandomNumber {
   private val IM = 139968
   private val IA = 3877
   private val IC = 29573
   private var seed = 42

   def scaledTo(max: Double) = {
      seed = (seed * IA + IC) % IM
      max * seed / IM
   }
}
