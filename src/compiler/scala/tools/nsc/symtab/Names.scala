/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.util.NameTransformer
import scala.io.UTF8Codec
import java.security.MessageDigest

/** The class <code>Names</code> ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 05/02/2005
 */
class Names {

// Operations -------------------------------------------------------------

  private final val HASH_SIZE  = 0x8000
  private final val HASH_MASK  = 0x7FFF
  private final val NAME_SIZE  = 0x20000

  private final val MaxFileNameLength = 255
  private final val MaxClassNameLength = MaxFileNameLength - 6 // leave space for ".class"

  final val nameDebug = false

  /** memory to store all names sequentially
   */
  var chrs: Array[Char] = new Array[Char](NAME_SIZE)
  private var nc = 0

  /** hashtable for finding term names quickly
   */
  private val termHashtable = new Array[Name](HASH_SIZE)

  /** hashtable for finding type names quickly
   */
  private val typeHashtable = new Array[Name](HASH_SIZE)

  /** the hashcode of a name
   */
  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int =
    if (len > 0)
      (len * (41 * 41 * 41) +
       cs(offset) * (41 * 41) +
       cs(offset + len - 1) * 41 +
       cs(offset + (len >> 1)))
    else 0;

  /** Is (the ASCII representation of) name at given index equal to
   *  <code>cs[offset..offset+len-1]</code>?
   *
   *  @param index  ...
   *  @param cs     ...
   *  @param offset ...
   *  @param len    ...
   *  @return       ...
   */
  private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
    var i = 0
    while ((i < len) && (chrs(index + i) == cs(offset + i)))
      i += 1;
    i == len
  }

  /** enter characters into chrs array
   */
  private def enterChars(cs: Array[Char], offset: Int, len: Int) {
    var i = 0
    while (i < len) {
      if (nc + i == chrs.length) {
        val newchrs = new Array[Char](chrs.length * 2)
        Array.copy(chrs, 0, newchrs, 0, chrs.length)
        chrs = newchrs
      }
      chrs(nc + i) = cs(offset + i)
      i += 1
    }
    if (len == 0) nc += 1
    else nc = nc + len
  }

  private lazy val md5 = MessageDigest.getInstance("MD5")

  private def toMD5(s: String, prefixSuffixLen: Int) = {
//  println("COMPACTIFY "+s)
    val cs: Array[Char] = s.toCharArray
    val bytes = new Array[Byte](cs.length * 4)
    val len = UTF8Codec.encode(cs, 0, bytes, 0, cs.length)
    md5.update(bytes, 0, len)
    val hash = md5.digest()
    val sb = new StringBuilder
    sb.append(cs, 0, prefixSuffixLen)
    sb.append("$$$$")
    for (i <- 0 until hash.length) {
      val b = hash(i)
      sb.append(((b >> 4) & 0xF).toHexString)
      sb.append((b & 0xF).toHexString)
    }
    sb.append("$$$$")
    sb.append(cs, len - prefixSuffixLen, prefixSuffixLen)
    sb.toString
  }

  def compactify(s: String): String =
    if (s.length <= MaxClassNameLength) s
    else toMD5(s, MaxClassNameLength / 4)

  /** Create a term name from the characters in <code>cs[offset..offset+len-1]</code>.
   *
   *  @param cs     ...
   *  @param offset ...
   *  @param len    ...
   *  @return       the created term name
   */
  def newTermName(cs: Array[Char], offset: Int, len: Int): Name = {
    val h = hashValue(cs, offset, len) & HASH_MASK
    var n = termHashtable(h)
    while ((n ne null) && (n.length != len || !equals(n.start, cs, offset, len)))
    n = n.next;
    if (n eq null) {
      n = new TermName(nc, len, h)
      enterChars(cs, offset, len)
    }
    n
  }

  /** create a term name from string
   */
  def newTermName(s: String): Name =
    newTermName(s.toCharArray(), 0, s.length())

  /** Create a term name from the UTF8 encoded bytes in <code>bs[offset..offset+len-1]</code>.
   *
   *  @param bs     ...
   *  @param offset ...
   *  @param len    ...
   *  @return       the created term name
   */
  def newTermName(bs: Array[Byte], offset: Int, len: Int): Name = {
    val cs = new Array[Char](bs.length)
    val nchrs = UTF8Codec.decode(bs, offset, cs, 0, len)
    newTermName(cs, 0, nchrs)
  }

  /** Create a type name from the characters in <code>cs[offset..offset+len-1]</code>.
   *
   *  @param cs     ...
   *  @param offset ...
   *  @param len    ...
   *  @return       the created type name
   */
  def newTypeName(cs: Array[Char], offset: Int, len: Int): Name =
    newTermName(cs, offset, len).toTypeName

  /** create a type name from string
   */
  def newTypeName(s: String): Name =
    newTermName(s).toTypeName

  /** Create a type name from the UTF8 encoded bytes in <code>bs[offset..offset+len-1]</code>.
   *
   *  @param bs     ...
   *  @param offset ...
   *  @param len    ...
   *  @return       the create type name
   */
  def newTypeName(bs: Array[Byte], offset: Int, len: Int): Name =
    newTermName(bs, offset, len).toTypeName


  def nameChars: Array[Char] = chrs

  implicit def view(s: String): Name = newTermName(s)

// Classes ----------------------------------------------------------------------

  /** The name class. */
  abstract class Name(index: Int, len: Int) extends Function1[Int, Char] {

    /** Index into name table */
    def start: Int = index

    /** next name in the same hash bucket
     */
    var next: Name = null

    /** return the length of this name
     */
    final def length: Int = len

    final def isEmpty = length == 0

    def isTermName: Boolean
    def isTypeName: Boolean
    def toTermName: Name
    def toTypeName: Name


    /** Copy bytes of this name to buffer <code>cs</code>, starting at position
     *  <code>offset</code>.
     *
     *  @param cs     ...
     *  @param offset ...
     */
    final def copyChars(cs: Array[Char], offset: Int) =
      Array.copy(chrs, index, cs, offset, len)

    /** return the ascii representation of this name
     */
    final def toChars: Array[Char] = {
      val cs = new Array[Char](len)
      copyChars(cs, 0)
      cs
    }

    /** return the string representation of this name
     */
    final override def toString(): String = new String(chrs, index, len)

    /** Write to UTF8 representation of this name to given character array.
     *  Start copying to index `to'. Return index of next free byte in array.
     *  Array must have enough remaining space for all bytes
     *  (i.e. maximally 3*length bytes).
     */
    final def copyUTF8(bs: Array[Byte], offset: Int): Int =
      UTF8Codec.encode(chrs, index, bs, offset, len)

    /** return the hash value of this name
     */
    final override def hashCode(): Int = index

    /** return the i'th Char of this name
     */
    final def apply(i: Int): Char = chrs(index + i)

    /** return the index of first occurrence of char c in this name, length if not found */
    final def pos(c: Char): Int = pos(c, 0)

    /** return the index of first occurrence of char c in this name, length if not found */
    final def pos(s: String): Int = pos(s, 0)

    /** return the index of the first occurrence of character <code>c</code> in
     *  this name from <code>start</code>, length if not found.
     *
     *  @param c     the character
     *  @param start ...
     *  @return      the index of the first occurrence of <code>c</code>
     */
    final def pos(c: Char, start: Int): Int = {
      var i = start
      while (i < len && chrs(index + i) != c) i += 1
      i
    }

    /** return the index of the first occurrence of nonempty string <code>s</code>
     *  in this name from <code>start</code>, length if not found.
     *
     *  @param s     the string
     *  @param start ...
     *  @return      the index of the first occurrence of <code>s</code>
     */
    final def pos(s: String, start: Int): Int = {
      var i = pos(s.charAt(0), start)
      while (i + s.length() <= len) {
        var j = 1
        while (s.charAt(j) == chrs(index + i + j)) {
          j += 1
          if (j == s.length()) return i
        }
        i = pos(s.charAt(0), i + 1)
      }
      len
    }

    /** return the index of last occurrence of char <code>c</code> in this
     *  name, <code>-1</code> if not found.
     *
     *  @param c the character
     *  @return  the index of the last occurrence of <code>c</code>
     */
    final def lastPos(c: Char): Int = lastPos(c, len - 1)

    final def lastPos(s: String): Int = lastPos(s, len - s.length())

    /** return the index of the last occurrence of char <code>c</code> in this
     *  name from <code>start</code>, <code>-1</code> if not found.
     *
     *  @param c     the character
     *  @param start ...
     *  @return      the index of the last occurrence of <code>c</code>
     */
    final def lastPos(c: Char, start: Int): Int = {
      var i = start
      while (i >= 0 && chrs(index + i) != c) i -= 1
      i
    }

    /** return the index of the last occurrence of string <code>s</code> in this
     *  name from <code>start</code>, <code>-1</code> if not found.
     *
     *  @param s     the string
     *  @param start ...
     *  @return      the index of the last occurrence of <code>s</code>
     */
    final def lastPos(s: String, start: Int): Int = {
      var i = lastPos(s.charAt(0), start)
      while (i >= 0) {
        var j = 1;
        while (s.charAt(j) == chrs(index + i + j)) {
          j += 1
          if (j == s.length()) return i;
        }
        i = lastPos(s.charAt(0), i - 1)
      }
      -s.length()
    }

    /** does this name start with prefix?
     */
    final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

    /** does this name start with prefix at given start index?
     */
    final def startsWith(prefix: Name, start: Int): Boolean = {
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == chrs(prefix.start + i))
        i += 1;
      i == prefix.length
    }

    /** does this name end with suffix?
     */
    final def endsWith(suffix: Name): Boolean = endsWith(suffix, len)

    /** does this name end with suffix just before given end index?
     */
    final def endsWith(suffix: Name, end: Int): Boolean = {
      var i = 1
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == chrs(suffix.start + suffix.length - i))
        i += 1;
      i > suffix.length
    }

    final def containsName(subname: Name): Boolean = {
      var start = 0
      val last = len - subname.length
      while (start <= last && !startsWith(subname, start)) start += 1
      start <= last
    }

    /** Return the subname with characters from start to end-1.
     *
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    def subName(from: Int, to: Int): Name

    /** Replace all occurrences of <code>from</code> by </code>to</code> in
     *  name; result is always a term name.
     *
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    def replace(from: Char, to: Char): Name = {
      val cs = new Array[Char](len)
      var i = 0
      while (i < len) {
        val ch = this(i)
        cs(i) = if (ch == from) to else ch
        i += 1
      }
      newTermName(cs, 0, len)
    }

    /** Replace operator symbols by corresponding <code>$op_name</code>.
     */
    def encode: Name = {
      val str = toString()
      val res = NameTransformer.encode(str)
      if (res == str) this
      else if (isTypeName) newTypeName(res)
      else newTermName(res)
    }

    /** Replace <code>$op_name</code> by corresponding operator symbol.
     */
    def decode: String = (
      NameTransformer.decode(toString()) +
      (if (nameDebug && isTypeName) "!" else ""))//debug
  }

  private class TermName(index: Int, len: Int, hash: Int) extends Name(index, len) {
    next = termHashtable(hash)
    termHashtable(hash) = this
    def isTermName: Boolean = true
    def isTypeName: Boolean = false
    def toTermName: Name = this
    def toTypeName = {
      val h = hashValue(chrs, index, len) & HASH_MASK
      var n = typeHashtable(h)
      while ((n ne null) && n.start != index)
        n = n.next;
      if (n eq null)
        n = new TypeName(index, len, h);
      n
    }
    def subName(from: Int, to: Int): Name =
      newTermName(chrs, start + from, to - from)
  }

  private class TypeName(index: Int, len: Int, hash: Int) extends Name(index, len) {
    next = typeHashtable(hash)
    typeHashtable(hash) = this
    def isTermName: Boolean = false
    def isTypeName: Boolean = true
    def toTermName: Name = {
      val h = hashValue(chrs, index, len) & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && n.start != index)
        n = n.next;
      if (n eq null)
        n = new TermName(index, len, h);
      n
    }
    def toTypeName: Name = this
    def subName(from: Int, to: Int): Name =
      newTypeName(chrs, start + from, to - from)
  }
}
