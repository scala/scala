/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.io.Codec
import java.security.MessageDigest
import language.implicitConversions

/** The class Names ...
 *
 *  @author  Martin Odersky
 *  @version 1.0, 05/02/2005
 */
trait Names extends api.Names {
  implicit def promoteTermNamesAsNecessary(name: Name): TermName = name.toTermName

// Operations -------------------------------------------------------------

  private final val HASH_SIZE  = 0x8000
  private final val HASH_MASK  = 0x7FFF
  private final val NAME_SIZE  = 0x20000

  final val nameDebug = false

  /** Memory to store all names sequentially. */
  var chrs: Array[Char] = new Array[Char](NAME_SIZE)
  private var nc = 0

  /** Hashtable for finding term names quickly. */
  private val termHashtable = new Array[TermName](HASH_SIZE)

  /** Hashtable for finding type names quickly. */
  private val typeHashtable = new Array[TypeName](HASH_SIZE)

  /** The hashcode of a name. */
  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int =
    if (len > 0)
      (len * (41 * 41 * 41) +
       cs(offset) * (41 * 41) +
       cs(offset + len - 1) * 41 +
       cs(offset + (len >> 1)))
    else 0;

  /** Is (the ASCII representation of) name at given index equal to
   *  cs[offset..offset+len-1]?
   */
  private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
    var i = 0
    while ((i < len) && (chrs(index + i) == cs(offset + i)))
      i += 1;
    i == len
  }

  /** Enter characters into chrs array. */
  private def enterChars(cs: Array[Char], offset: Int, len: Int) {
    var i = 0
    while (i < len) {
      if (nc + i == chrs.length) {
        val newchrs = new Array[Char](chrs.length * 2)
        compat.Platform.arraycopy(chrs, 0, newchrs, 0, chrs.length)
        chrs = newchrs
      }
      chrs(nc + i) = cs(offset + i)
      i += 1
    }
    if (len == 0) nc += 1
    else nc = nc + len
  }

  /** Create a term name from the characters in cs[offset..offset+len-1]. */
  def newTermName(cs: Array[Char], offset: Int, len: Int): TermName =
    newTermName(cs, offset, len, cachedString = null)

  def newTermName(cs: Array[Char]): TermName = newTermName(cs, 0, cs.length)
  def newTypeName(cs: Array[Char]): TypeName = newTypeName(cs, 0, cs.length)

  /** Create a term name from the characters in cs[offset..offset+len-1].
   *  TODO - have a mode where name validation is performed at creation time
   *  (e.g. if a name has the string "$class" in it, then fail if that
   *  string is not at the very end.)
   */
  protected def newTermName(cs: Array[Char], offset: Int, len: Int, cachedString: String): TermName = {
    val h = hashValue(cs, offset, len) & HASH_MASK
    var n = termHashtable(h)
    while ((n ne null) && (n.length != len || !equals(n.start, cs, offset, len)))
      n = n.next

    if (n ne null) n
    else {
      // The logic order here is future-proofing against the possibility
      // that name.toString will become an eager val, in which case the call
      // to enterChars cannot follow the construction of the TermName.
      val ncStart = nc
      enterChars(cs, offset, len)
      if (cachedString ne null) new TermName_S(ncStart, len, h, cachedString)
      else new TermName_R(ncStart, len, h)
    }
  }
  protected def newTypeName(cs: Array[Char], offset: Int, len: Int, cachedString: String): TypeName =
    newTermName(cs, offset, len, cachedString).toTypeName

  /** Create a term name from string. */
  def newTermName(s: String): TermName = newTermName(s.toCharArray(), 0, s.length(), null)

  /** Create a type name from string. */
  def newTypeName(s: String): TypeName = newTermName(s).toTypeName

  /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
  def newTermName(bs: Array[Byte], offset: Int, len: Int): TermName = {
    val chars = Codec.fromUTF8(bs, offset, len)
    newTermName(chars, 0, chars.length)
  }

  def newTermNameCached(s: String): TermName =
    newTermName(s.toCharArray(), 0, s.length(), cachedString = s)

  def newTypeNameCached(s: String): TypeName =
    newTypeName(s.toCharArray(), 0, s.length(), cachedString = s)

  /** Create a type name from the characters in cs[offset..offset+len-1]. */
  def newTypeName(cs: Array[Char], offset: Int, len: Int): TypeName =
    newTermName(cs, offset, len, cachedString = null).toTypeName

  /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
  def newTypeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
    newTermName(bs, offset, len).toTypeName

  def nameChars: Array[Char] = chrs
  @deprecated("", "2.9.0") def view(s: String): TermName = newTermName(s)

// Classes ----------------------------------------------------------------------

  /** The name class.
   *  TODO - resolve schizophrenia regarding whether to treat Names as Strings
   *  or Strings as Names.  Give names the key functions the absence of which
   *  make people want Strings all the time.
   */
  sealed abstract class Name(protected val index: Int, protected val len: Int) extends AbsName with Function1[Int, Char] {
    type ThisNameType <: Name
    protected[this] def thisName: ThisNameType

    /** Index into name table */
    def start: Int = index

    /** The next name in the same hash bucket. */
    def next: ThisNameType

    /** The length of this name. */
    final def length: Int = len
    final def isEmpty = length == 0
    final def nonEmpty = !isEmpty

    def nameKind: String
    def isTermName: Boolean
    def isTypeName: Boolean
    def toTermName: TermName
    def toTypeName: TypeName
    def companionName: Name
    def bothNames: List[Name] = List(toTermName, toTypeName)

    /** Return the subname with characters from from to to-1. */
    def subName(from: Int, to: Int): ThisNameType

    /** Return a new name of the same variety. */
    def newName(str: String): ThisNameType

    /** Return a new name based on string transformation. */
    def mapName(f: String => String): ThisNameType = newName(f(toString))

    /** Copy bytes of this name to buffer cs, starting at position `offset`. */
    final def copyChars(cs: Array[Char], offset: Int) =
      compat.Platform.arraycopy(chrs, index, cs, offset, len)

    /** @return the ascii representation of this name */
    final def toChars: Array[Char] = {
      val cs = new Array[Char](len)
      copyChars(cs, 0)
      cs
    }

    /** Write to UTF8 representation of this name to given character array.
     *  Start copying to index `to`. Return index of next free byte in array.
     *  Array must have enough remaining space for all bytes
     *  (i.e. maximally 3*length bytes).
     */
    final def copyUTF8(bs: Array[Byte], offset: Int): Int = {
      val bytes = Codec.toUTF8(chrs, index, len)
      compat.Platform.arraycopy(bytes, 0, bs, offset, bytes.length)
      offset + bytes.length
    }

    /** @return the hash value of this name */
    final override def hashCode(): Int = index

    // Presently disabled.
    // override def equals(other: Any) = paranoidEquals(other)
    private def paranoidEquals(other: Any): Boolean = {
      val cmp = this eq other.asInstanceOf[AnyRef]
      if (cmp || !nameDebug)
        return cmp

      other match {
        case x: String  =>
          Console.println("Compared " + debugString + " and String '" + x + "'")
        case x: Name    =>
          if (this.isTermName != x.isTermName) {
            val panic = this.toTermName == x.toTermName
            Console.println("Compared '%s' and '%s', one term, one type.%s".format(this, x,
              if (panic) "  And they contain the same name string!"
              else ""
            ))
          }
        case _ =>
      }
      false
    }

    /** @return the i'th Char of this name */
    final def apply(i: Int): Char = chrs(index + i)

    /** @return the index of first occurrence of char c in this name, length if not found */
    final def pos(c: Char): Int = pos(c, 0)

    /** @return the index of first occurrence of char c in this name, length if not found */
    final def pos(s: String): Int = pos(s, 0)

    /** Returns the index of the first occurrence of character c in
     *  this name from start, length if not found.
     *
     *  @param c     the character
     *  @param start ...
     *  @return      the index of the first occurrence of c
     */
    final def pos(c: Char, start: Int): Int = {
      var i = start
      while (i < len && chrs(index + i) != c) i += 1
      i
    }

    /** Returns the index of the first occurrence of nonempty string s
     *  in this name from start, length if not found.
     *
     *  @param s     the string
     *  @param start ...
     *  @return      the index of the first occurrence of s
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

    /** Returns the index of last occurrence of char c in this
     *  name, -1 if not found.
     *
     *  @param c the character
     *  @return  the index of the last occurrence of c
     */
    final def lastPos(c: Char): Int = lastPos(c, len - 1)

    final def lastPos(s: String): Int = lastPos(s, len - s.length)

    /** Returns the index of the last occurrence of char c in this
     *  name from start, -1 if not found.
     *
     *  @param c     the character
     *  @param start ...
     *  @return      the index of the last occurrence of c
     */
    final def lastPos(c: Char, start: Int): Int = {
      var i = start
      while (i >= 0 && chrs(index + i) != c) i -= 1
      i
    }

    /** Returns the index of the last occurrence of string s in this
     *  name from start, -1 if not found.
     *
     *  @param s     the string
     *  @param start ...
     *  @return      the index of the last occurrence of s
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

    /** Does this name start with prefix? */
    final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

    /** Does this name start with prefix at given start index? */
    final def startsWith(prefix: Name, start: Int): Boolean = {
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == chrs(prefix.start + i))
        i += 1;
      i == prefix.length
    }

    /** Does this name end with suffix? */
    final def endsWith(suffix: Name): Boolean = endsWith(suffix, len)

    /** Does this name end with suffix just before given end index? */
    final def endsWith(suffix: Name, end: Int): Boolean = {
      var i = 1
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == chrs(suffix.start + suffix.length - i))
        i += 1;
      i > suffix.length
    }

    final def containsName(subname: String): Boolean = containsName(newTermName(subname))
    final def containsName(subname: Name): Boolean = {
      var start = 0
      val last = len - subname.length
      while (start <= last && !startsWith(subname, start)) start += 1
      start <= last
    }
    final def containsChar(ch: Char): Boolean = {
      var i = index
      val max = index + len
      while (i < max) {
        if (chrs(i) == ch)
          return true
        i += 1
      }
      false
    }

    /** Some thoroughly self-explanatory convenience functions.  They
     *  assume that what they're being asked to do is known to be valid.
     */
    final def startChar: Char                   = apply(0)
    final def endChar: Char                     = apply(len - 1)
    final def startsWith(char: Char): Boolean   = len > 0 && startChar == char
    final def startsWith(name: String): Boolean = startsWith(newTermName(name))
    final def endsWith(char: Char): Boolean     = len > 0 && endChar == char
    final def endsWith(name: String): Boolean   = endsWith(newTermName(name))

    def dropRight(n: Int): ThisNameType = subName(0, len - n)
    def drop(n: Int): ThisNameType = subName(n, len)
    def stripSuffix(suffix: Name): ThisNameType =
      if (this endsWith suffix) dropRight(suffix.length) else thisName

    def indexOf(ch: Char) = {
      val idx = pos(ch)
      if (idx == length) -1 else idx
    }
    def indexOf(ch: Char, fromIndex: Int) = {
      val idx = pos(ch, fromIndex)
      if (idx == length) -1 else idx
    }
    def lastIndexOf(ch: Char) = lastPos(ch)
    def lastIndexOf(ch: Char, fromIndex: Int) = lastPos(ch, fromIndex)

    /** Replace all occurrences of `from` by `to` in
     *  name; result is always a term name.
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

    /** TODO - reconcile/fix that encode returns a Name but
     *  decode returns a String.
     */

    /** !!! Duplicative but consistently named.
     */
    def decoded: String = decode
    def encoded: String = "" + encode
    // def decodedName: ThisNameType = newName(decoded)
    def encodedName: ThisNameType = encode

    /** Replace operator symbols by corresponding \$op_name. */
    def encode: ThisNameType = {
      val str = toString
      val res = NameTransformer.encode(str)
      if (res == str) thisName else newName(res)
    }

    /** Replace \$op_name by corresponding operator symbol. */
    def decode: String = {
      if (this containsChar '$') {
        val str = toString
        val res = NameTransformer.decode(str)
        if (res == str) str
        else res
      }
      else toString
    }

    /** TODO - find some efficiency. */
    def append(ch: Char)        = newName("" + this + ch)
    def append(suffix: String)  = newName("" + this + suffix)
    def append(suffix: Name)    = newName("" + this + suffix)
    def prepend(ch: Char)       = newName("" + ch + this)
    def prepend(prefix: String) = newName("" + prefix + this)
    def prepend(prefix: Name)   = newName("" + prefix + this)

    def decodedName: ThisNameType = newName(decode)
    def isOperatorName: Boolean = decode != toString
    def longString: String      = nameKind + " " + decode
    def debugString = { val s = decode ; if (isTypeName) s + "!" else s }
  }

  /** A name that contains no operator chars nor dollar signs.
   *  TODO - see if it's any faster to do something along these lines.
   *  Cute: now that exhaustivity kind of works, the mere presence of
   *  this trait causes TermName and TypeName to stop being exhaustive.
   *  Commented out.
   */
  // trait AlphaNumName extends Name {
  //   final override def encode         = thisName
  //   final override def decodedName    = thisName
  //   final override def decode         = toString
  //   final override def isOperatorName = false
  // }

  /** TermName_S and TypeName_S have fields containing the string version of the name.
   *  TermName_R and TypeName_R recreate it each time toString is called.
   */
  private class TermName_S(index0: Int, len0: Int, hash: Int, override val toString: String) extends TermName(index0, len0, hash) {
    protected def createCompanionName(h: Int): TypeName = new TypeName_S(index, len, h, toString)
    override def newName(str: String): TermName = newTermNameCached(str)
  }
  private class TypeName_S(index0: Int, len0: Int, hash: Int, override val toString: String) extends TypeName(index0, len0, hash) {
    protected def createCompanionName(h: Int): TermName = new TermName_S(index, len, h, toString)
    override def newName(str: String): TypeName = newTypeNameCached(str)
  }

  private class TermName_R(index0: Int, len0: Int, hash: Int) extends TermName(index0, len0, hash) {
    protected def createCompanionName(h: Int): TypeName = new TypeName_R(index, len, h)
    override def toString = new String(chrs, index, len)
  }

  private class TypeName_R(index0: Int, len0: Int, hash: Int) extends TypeName(index0, len0, hash) {
    protected def createCompanionName(h: Int): TermName = new TermName_R(index, len, h)
    override def toString = new String(chrs, index, len)
  }

  sealed abstract class TermName(index0: Int, len0: Int, hash: Int) extends Name(index0, len0) {
    type ThisNameType = TermName
    protected[this] def thisName: TermName = this

    var next: TermName = termHashtable(hash)
    termHashtable(hash) = this
    def isTermName: Boolean = true
    def isTypeName: Boolean = false
    def toTermName: TermName = this
    def toTypeName: TypeName = {
      val h = hashValue(chrs, index, len) & HASH_MASK
      var n = typeHashtable(h)
      while ((n ne null) && n.start != index)
        n = n.next

      if (n ne null) n
      else createCompanionName(h)
    }
    def newName(str: String): TermName = newTermName(str)
    def companionName: TypeName = toTypeName
    def subName(from: Int, to: Int): TermName =
      newTermName(chrs, start + from, to - from)

    def nameKind = "term"
    protected def createCompanionName(h: Int): TypeName
  }

  sealed abstract class TypeName(index0: Int, len0: Int, hash: Int) extends Name(index0, len0) {
    type ThisNameType = TypeName
    protected[this] def thisName: TypeName = this

    var next: TypeName = typeHashtable(hash)
    typeHashtable(hash) = this
    def isTermName: Boolean = false
    def isTypeName: Boolean = true
    def toTermName: TermName = {
      val h = hashValue(chrs, index, len) & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && n.start != index)
        n = n.next

      if (n ne null) n
      else createCompanionName(h)
    }
    def toTypeName: TypeName = this
    def newName(str: String): TypeName = newTypeName(str)
    def companionName: TermName = toTermName
    def subName(from: Int, to: Int): TypeName =
      newTypeName(chrs, start + from, to - from)

    def nameKind = "type"
    override def decode = if (nameDebug) super.decode + "!" else super.decode
    protected def createCompanionName(h: Int): TermName
  }
}
