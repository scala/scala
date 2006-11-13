/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab

import scala.tools.nsc.util.NameTransformer
import scala.tools.util.UTF8Codec

/** The class <code>Names</code> ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 05/02/2005
 */
class Names {

// Operations -------------------------------------------------------------

  private val HASH_SIZE = 0x8000
  private val HASH_MASK = 0x7FFF
  private val NAME_SIZE = 0x20000

  final val nameDebug = false

  /** memory to store all names sequentially
   */
  var chrs: Array[char] = new Array[char](NAME_SIZE)
  private var nc = 0

  /** hashtable for finding term names quickly
   */
  private val termHashtable = new Array[Name](HASH_SIZE)

  /** hashtable for finding type names quickly
   */
  private val typeHashtable = new Array[Name](HASH_SIZE)

  /** the hashcode of a name
   */
  private def hashValue(cs: Array[char], offset: int, len: int): int =
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
  private def equals(index: int, cs: Array[char], offset: int, len: int): boolean = {
    var i = 0
    while ((i < len) && (chrs(index + i) == cs(offset + i)))
      i = i + 1;
    i == len
  }

  /** enter characters into chrs array
   */
  private def enterChars(cs: Array[char], offset: int, len: int): unit = {
    var i = 0
    while (i < len) {
      if (nc + i == chrs.length) {
        val newchrs = new Array[char](chrs.length * 2)
        Array.copy(chrs, 0, newchrs, 0, chrs.length)
        chrs = newchrs
      }
      chrs(nc + i) = cs(offset + i)
      i = i + 1
    }
    if (len == 0) nc = nc + 1
    else nc = nc + len
  }

  /** Create a term name from the characters in <code>cs[offset..offset+len-1]</code>.
   *
   *  @param cs     ...
   *  @param offset ...
   *  @param len    ...
   *  @return       the created term name
   */
  def newTermName(cs: Array[char], offset: int, len: int): Name = {
    val h = hashValue(cs, offset, len) & HASH_MASK;
    var n = termHashtable(h);
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
  def newTermName(bs: Array[byte], offset: int, len: int): Name = {
    val cs = new Array[char](bs.length)
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
  def newTypeName(cs: Array[char], offset: int, len: int): Name =
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
  def newTypeName(bs: Array[byte], offset: int, len: int): Name =
    newTermName(bs, offset, len).toTypeName


  def nameChars: Array[char] = chrs

  implicit def view(s: String): Name = newTermName(s)

// Classes ----------------------------------------------------------------------

  /** The name class. */
  abstract class Name(index: int, len: int) extends Function1[int, char] {

    /** Index into name table */
    def start: int = index

    /** next name in the same hash bucket
     */
    var next: Name = null

    /** return the length of this name
     */
    final def length: int = len

    final def isEmpty = length == 0

    def isTermName: boolean
    def isTypeName: boolean
    def toTermName: Name
    def toTypeName: Name


    /** Copy bytes of this name to buffer <code>cs</code>, starting at position
     *  <code>offset</code>.
     *
     *  @param cs     ...
     *  @param offset ...
     */
    final def copyChars(cs: Array[char], offset: int) =
      Array.copy(chrs, index, cs, offset, len)

    /** return the ascii representation of this name
     */
    final def toChars = {
      val cs = new Array[char](len)
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
    final def copyUTF8(bs: Array[byte], offset: int): int =
      UTF8Codec.encode(chrs, index, bs, offset, len)

    /** return the hash value of this name
     */
    final override def hashCode(): int = index

    /** return the i'th char of this name
     */
    final def apply(i: int): char = chrs(index + i)

    /** return the index of first occurrence of char c in this name, length if not found */
    final def pos(c: char): int = pos(c, 0)

    /** return the index of first occurrence of char c in this name, length if not found */
    final def pos(s: String): int = pos(s, 0)

    /** return the index of the first occurrence of character <code>c</code> in
     *  this name from <code>start</code>, length if not found.
     *
     *  @param c     the character
     *  @param start ...
     *  @return      the index of the first occurrence of <code>c</code>
     */
    final def pos(c: char, start: int): int = {
      var i = start
      while (i < len && chrs(index + i) != c) i = i + 1
      i
    }

    /** return the index of the first occurrence of nonempty string <code>s</code>
     *  in this name from <code>start</code>, length if not found.
     *
     *  @param s     the string
     *  @param start ...
     *  @return      the index of the first occurrence of <code>s</code>
     */
    final def pos(s: String, start: int): int = {
      var i = pos(s.charAt(0), start)
      while (i + s.length() <= len) {
        var j = 1
        while (s.charAt(j) == chrs(index + i + j)) {
          j = j + 1
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
    final def lastPos(c: char): int = lastPos(c, len - 1)

    final def lastPos(s: String): int = lastPos(s, len - s.length())

    /** return the index of the last occurrence of char <code>c</code> in this
     *  name from <code>start</code>, <code>-1</code> if not found.
     *
     *  @param c     the character
     *  @param start ...
     *  @return      the index of the last occurrence of <code>c</code>
     */
    final def lastPos(c: char, start: int): int = {
      var i = start
      while (i >= 0 && chrs(index + i) != c) i = i - 1
      i
    }

    /** return the index of the last occurrence of string <code>s</code> in this
     *  name from <code>start</code>, <code>-1</code> if not found.
     *
     *  @param s     the string
     *  @param start ...
     *  @return      the index of the last occurrence of <code>s</code>
     */
    final def lastPos(s: String, start: int): int = {
      var i = lastPos(s.charAt(0), start)
      while (i >= 0) {
        var j = 1;
        while (s.charAt(j) == chrs(index + i + j)) {
          j = j + 1;
          if (j == s.length()) return i;
        }
        i = lastPos(s.charAt(0), i - 1)
      }
      -s.length()
    }

    /** does this name start with prefix?
     */
    final def startsWith(prefix: Name): boolean = startsWith(prefix, 0)

    /** does this name start with prefix at given start index?
     */
    final def startsWith(prefix: Name, start: int): boolean = {
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == chrs(prefix.start + i))
        i = i + 1;
      i == prefix.length
    }

    /** does this name end with suffix?
     */
    final def endsWith(suffix: Name): boolean = endsWith(suffix, len)

    /** does this name end with suffix just before given end index?
     */
    final def endsWith(suffix: Name, end: int): boolean = {
      var i = 1
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == chrs(suffix.start + suffix.length - i))
        i = i + 1;
      i > suffix.length
    }

    /** Return the subname with characters from start to end-1.
     *
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    def subName(from: int, to: int): Name

    /** Replace all occurrences of <code>from</code> by </code>to</code> in
     *  name; result is always a term name.
     *
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    def replace(from: char, to: char): Name = {
      val cs = new Array[char](len)
      var i = 0
      while (i < len) {
        val ch = this(i)
        cs(i) = if (ch == from) to else ch
        i = i + 1
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

  private class TermName(index: int, len: int, hash: int) extends Name(index, len) {
    next = termHashtable(hash)
    termHashtable(hash) = this
    def isTermName: boolean = true
    def isTypeName: boolean = false
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
    def subName(from: int, to: int): Name =
      newTermName(chrs, start + from, to - from)
  }

  private class TypeName(index: int, len: int, hash: int) extends Name(index, len) {
    next = typeHashtable(hash)
    typeHashtable(hash) = this
    def isTermName: boolean = false
    def isTypeName: boolean = true
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
    def subName(from: int, to: int): Name =
      newTypeName(chrs, start + from, to - from)
  }
}
