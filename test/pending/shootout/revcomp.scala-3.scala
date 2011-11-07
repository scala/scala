/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import java.io._
import scala.collection.mutable.Stack

object revcomp { 
   def main(args: Array[String]) = {
      val out = new FastaOutputStream(System.out)
      val in = new FastaInputStream(System.in)

      out.writeReverseComplement( in.readSequenceStack )
      out.writeReverseComplement( in.readSequenceStack )
      out.writeReverseComplement( in.readSequenceStack )

      in.close
      out.close
   } 
}


trait FastaByteStream {
   val nl = '\n'.toByte  

   type Line = Array[Byte]
   type LineStack = Stack[Line]
}


// extend the Java BufferedInputStream class

final class FastaInputStream(in: InputStream) 
      extends BufferedInputStream(in) with FastaByteStream {

   val gt = '>'.toByte
   val sc = ';'.toByte

   def readSequenceStack(): Pair[Line,LineStack] = {
      var header: Line = null
      val lines: LineStack = new Stack

      var line = readLine()
      while (line != null) {
         val c = line(0)
         if (c == gt){                       // '>'
            if (header == null){
               header = line
            } else {
               pos = pos - line.length - 1   // reposition to start of line
               return Pair(header,lines)
            }
         } else {
            if (c != sc) lines push line       // ';'
         }
         line = readLine()
      }
      return Pair(header,lines)
   }

   def readLine() = {
      var bytes: Line = null
      if (in == null) bytes
      else {
         mark(128)                      // mark the start of the line
         if (count == 0) read()         // fill buffer
        
         var i = markpos
         while (i < count && buf(i) != nl) i = i + 1

         if (i >= count){               // line extends past end of buffer
            pos = i; read(); i = pos;   // fill buffer again
            while (i < count && buf(i) != nl) i = i + 1
         }

         if (i < count){     
            bytes = new Array(i - markpos)
            System.arraycopy(buf, markpos, bytes, 0, i - markpos);
            pos = i+1
         }          
      }
      bytes
   }
}


// extend the Java BufferedOutputStream class

final class FastaOutputStream(in: OutputStream) 
      extends BufferedOutputStream(in) with FastaByteStream {

   private val IUB = IUBCodeComplements

   private def IUBCodeComplements() = {
      val code = "ABCDGHKMNRSTVWYabcdghkmnrstvwy".getBytes
      val comp = "TVGHCDMKNYSABWRTVGHCDMKNYSABWR".getBytes
      val iub: Array[Byte] = new Array( 'z'.toByte )

      for (indexValue <- code zip comp)
         indexValue match { case Pair(i,v) => iub(i) = v }

      iub
   }

   def writeReverseComplement(sequence: Pair[Line,LineStack]) = {

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

      sequence match {
         case Pair(header,lines) => {

            write(header); write(nl)

            val k = if (lines.isEmpty) 0 else lines.top.length
            val LineLength = 60
            val isSplitLine = k < LineLength
            var isFirstLine = true

            while (!lines.isEmpty) {
               val line = lines.pop
               inplaceComplementReverse(line)
        
               if (isSplitLine){
                  if (isFirstLine){ write(line); isFirstLine = false } 
                  else { write(line,0,LineLength-k); write(nl); write(line,LineLength-k,k) }
               } 
               else { write(line); write(nl) }
            }

            if (isSplitLine && !isFirstLine) write(nl)
         }
      }
   }

}
