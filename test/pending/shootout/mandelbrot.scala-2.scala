/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

// This test is in pending because it fails on windows only,
// but partest's output and the fact that this test outputs in
// binary makes it a challenge to debug remotely.  However,
// it's easy to guess that it has to do with the BufferedOutputStream
// and some kind of windows-specific damage that requires an extra
// flush, or different line-ending characters, or any of the various
// write-once-know-quirks-everywhere aspects of java i/o.
//
//   [partest] testing: [...]\files\shootout\mandelbrot.scala-2.scala                [FAILED]
//   [partest] P4
//   [partest] 200 200
//   [partest] 
// ^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^B^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@
// ^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@
// [etc]

import java.io.BufferedOutputStream

object mandelbrot { 
   def main(args: Array[String]) = {
      val side = Integer.parseInt(args(0))
      val limitSquared = 4.0
      val max = 50
      var bits = 0
      var bitnum = 0
      val w = new BufferedOutputStream(System.out)

      Console.println("P4\n" + side + " " + side)

      var y = 0
      while (y < side){

         var x = 0
         while (x < side){

            val cr = 2.0 * x / side - 1.5
            val ci = 2.0 * y / side - 1.0

            var zr = 0.0; var zi = 0.0
            var tr = 0.0; var ti = 0.0

            var j = max
            do {
               zi = 2.0 * zr * zi + ci
               zr = tr - ti + cr
               ti = zi*zi
               tr = zr*zr

               j = j - 1
            } while (!(tr + ti > limitSquared) && j > 0)


            bits = bits << 1
            if (!(tr + ti > limitSquared)) bits = bits + 1
            bitnum = bitnum + 1

            if (x == side - 1){
               bits = bits << (8 - bitnum)
               bitnum = 8
            }

            if (bitnum == 8){
               w.write(bits.toByte)
               bits = 0
               bitnum = 0
            }

            x = x + 1
         }
         y = y + 1
      }
      w.close
   } 
}
