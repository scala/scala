/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


package scala.tools.nsc.symtab.classfile

import scala.tools.nsc.io.{AbstractFile, PlainFile, ZipArchive}

import java.io.{File, FileInputStream, IOException};

/** this class reads files byte per byte. Only used by ClassFileParser
 */
class AbstractFileReader(val file: AbstractFile) {

  /** the buffer containing the file
   */
  val buf: Array[Byte] = file match {
    case p:PlainFile =>
      assert(!file.isDirectory, "cannot read directory '" + this + "'");
      val in = new FileInputStream(p.file);
      var rest: Int = p.file.length().toInt;
      val bbuf: Array[Byte] = new Array[Byte](rest);
      while (rest > 0) {
        val res = in.read(bbuf, bbuf.length - rest, rest);
        if (res == -1)
          throw new IOException("read error");
        rest = rest - res;
      }
      in.close();
      bbuf
    case z:ZipArchive#FileEntry => // zipfileentry
      val in = z.getArchive.getInputStream(z.entry)
      var rest: Int = z.entry.getSize().toInt;
      val buf = new Array[Byte](rest);
      while (rest > 0) {
        val res = in.read(buf, buf.length - rest, rest);
        if (res == -1)
          throw new IOException("read error");
        rest = rest - res;
      }
      in.close();
      buf
    case _ =>
      error("internal error: do not know how to get bytes of "+file)
  }

  /** the current input pointer
   */
  var bp: Int = 0

  /** return byte at offset 'pos'
   */
  def byteAt(pos: Int): Byte = return buf(pos)

  /** read a byte
   */
  def nextByte: Byte = {
    val b = buf(bp)
    bp = bp + 1
    b
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = {
    bp = bp + len
    buf.subArray(bp - len, bp)
  }

  /** read a character
   */
  def nextChar: Char = {
    (((nextByte & 0xff) << 8) + (nextByte & 0xff)).toChar
  }

  /** read an integer
   */
  def nextInt: Int =
    ((nextByte & 0xff) << 24) + ((nextByte & 0xff) << 16) +
    ((nextByte & 0xff) <<  8) +  (nextByte & 0xff)


  /** extract a character at position bp from buf
   */
  def getChar(mybp: Int): Char =
    (((buf(mybp) & 0xff) << 8) + (buf(mybp+1) & 0xff)).toChar

  /** extract an integer at position bp from buf
   */
  def getInt(mybp: Int): Int =
    ((buf(mybp  ) & 0xff) << 24) + ((buf(mybp+1) & 0xff) << 16) +
    ((buf(mybp+2) & 0xff) << 8) + (buf(mybp+3) & 0xff)

  /** extract a long integer at position bp from buf
   */
  def getLong(mybp: Int): Long =
    (getInt(mybp).toLong << 32) + (getInt(mybp + 4) & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(mybp: Int): Float = Float.intBitsToFloat(getInt(mybp))

  /** extract a double at position bp from buf
   */
  def getDouble(mybp: Int): Double = Double.longBitsToDouble(getLong(mybp))

  /** skip next 'n' bytes
   */
  def skip(n: Int): Unit = bp = bp + n

}
