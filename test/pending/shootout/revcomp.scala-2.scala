/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import java.io._
import scala.collection.mutable.Stack

object revcomp {

   val IUB = IUBCodeComplements

   def IUBCodeComplements() = {
      val code = "ABCDGHKMNRSTVWYabcdghkmnrstvwy".getBytes
      val comp = "TVGHCDMKNYSABWRTVGHCDMKNYSABWR".getBytes
      val a: Array[Byte] = new Array( 'z'.toByte )

      for (indexValue <- code zip comp)
         indexValue match { case (i,v) => a(i) = v }

      a
   }


   type LineStack = Stack[Array[Byte]]

   def main(args: Array[String]) = {
      val r = new BufferedReader(new InputStreamReader(System.in))
      val w = new BufferedOutputStream(System.out)

      var lines: LineStack = new Stack
      var desc = ""

      var line = r.readLine
      while (line != null) {
         val c = line.charAt(0)
         if (c == '>'){
            if (desc.length > 0){
               complementReverseWrite(desc, lines, w)
               lines = new Stack
            }
            desc = line
         } else {
            if (c != ';') lines += line.getBytes
         }
         line = r.readLine
      }
      r.close

      if (desc.length > 0) complementReverseWrite(desc, lines, w)
      w.close
   }


   def complementReverseWrite(desc: String, lines: LineStack,
         w: BufferedOutputStream) = {

      def inplaceComplementReverse(b: Array[Byte]) = {
         var i = 0
         var j = b.length - 1
         while (i < j){
            val swap = b(i)
            b(i) = IUB( b(j) )
            b(j) = IUB( swap )
            i = i + 1
            j = j - 1
         }
         if (i == j) b(i) = IUB( b(i) )
      }

      val nl = '\n'.toByte
      w.write(desc.getBytes); w.write(nl)

      val n = 60
      val k = if (lines.isEmpty) 0 else lines.top.length
      val isSplitLine = k < n
      var isFirstLine = true

      while (!lines.isEmpty) {
        val line = lines.pop
        inplaceComplementReverse(line)

        if (isSplitLine){
           if (isFirstLine){ w.write(line); isFirstLine = false }
           else { w.write(line,0,n-k); w.write(nl); w.write(line,n-k,k) }
        }
        else { w.write(line); w.write(nl) }
      }
      if (isSplitLine && !isFirstLine) w.write(nl)
   }

}
