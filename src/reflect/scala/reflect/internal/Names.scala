/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.language.implicitConversions

import scala.io.Codec

trait Names extends api.Names {
  private final val HASH_SIZE  = 0x8000
  private final val HASH_MASK  = 0x7FFF
  private final val NAME_SIZE  = 0x20000

  final val nameDebug = false

  // Ideally we would just synchronize unconditionally and let HotSpot's Biased Locking
  // kick in in the compiler universe, where access to the lock is single threaded. But,
  // objects created in the first 4seconds of the JVM startup aren't eligible for biased
  // locking.
  //
  // We might also be able to accept the performance hit, but we don't have tools to
  // detect performance regressions.
  //
  // Discussion: https://groups.google.com/forum/#!search/biased$20scala-internals/scala-internals/0cYB7SkJ-nM/47MLhsgw8jwJ
  protected def synchronizeNames: Boolean = false
  private val nameLock: Object = new Object

  /** Memory to store all names sequentially. */
  var chrs: Array[Char] = new Array[Char](NAME_SIZE)
  private var nc = 0

  /** Hashtable for finding term names quickly. */
  private val termHashtable = new Array[TermName](HASH_SIZE)

  /** Hashtable for finding type names quickly. */
  private val typeHashtable = new Array[TypeName](HASH_SIZE)

  /**
   * The hashcode of a name depends on the first, the last and the middle character,
   * and the length of the name.
   */
  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int =
    if (len > 0)
      (len * (41 * 41 * 41) +
       cs(offset) * (41 * 41) +
       cs(offset + len - 1) * 41 +
       cs(offset + (len >> 1)))
    else 0

  /** Is (the ASCII representation of) name at given index equal to
   *  cs[offset..offset+len-1]?
   */
  private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
    var i = 0
    while ((i < len) && (chrs(index + i) == cs(offset + i)))
      i += 1
    i == len
  }

  /** Enter characters into chrs array. */
  private def enterChars(cs: Array[Char], offset: Int, len: Int) {
    var i = 0
    while (i < len) {
      if (nc + i == chrs.length) {
        val newchrs = new Array[Char](chrs.length * 2)
        scala.compat.Platform.arraycopy(chrs, 0, newchrs, 0, chrs.length)
        chrs = newchrs
      }
      chrs(nc + i) = cs(offset + i)
      i += 1
    }
    if (len == 0) nc += 1
    else nc = nc + len
  }

  /** Create a term name from the characters in cs[offset..offset+len-1]. */
  final def newTermName(cs: Array[Char], offset: Int, len: Int): TermName =
    newTermName(cs, offset, len, cachedString = null)

  final def newTermName(cs: Array[Char]): TermName = newTermName(cs, 0, cs.length)

  final def newTypeName(cs: Array[Char]): TypeName = newTypeName(cs, 0, cs.length)

  /** Create a term name from the characters in cs[offset..offset+len-1].
   *  TODO - have a mode where name validation is performed at creation time
   *  (e.g. if a name has the string "$class" in it, then fail if that
   *  string is not at the very end.)
   *
   *  @param len0 the length of the name. Negative lengths result in empty names.
   */
  final def newTermName(cs: Array[Char], offset: Int, len0: Int, cachedString: String): TermName = {
    def body = {
      require(offset >= 0, "offset must be non-negative, got " + offset)
      val len = math.max(len0, 0)
      val h = hashValue(cs, offset, len) & HASH_MASK
      var n = termHashtable(h)
      while ((n ne null) && (n.length != len || !equals(n.start, cs, offset, len)))
        n = n.next

      if (n ne null) n
      else {
        // The logic order here is future-proofing against the possibility
        // that name.toString will become an eager val, in which case the call
        // to enterChars cannot follow the construction of the TermName.
        var startIndex = 0
        if (cs == chrs) {
          // Optimize for subName, the new name is already stored in chrs
          startIndex = offset
        } else {
          startIndex = nc
          enterChars(cs, offset, len)
        }
        val next = termHashtable(h)
        val termName =
          if (cachedString ne null) new TermName_S(startIndex, len, next, cachedString)
          else new TermName_R(startIndex, len, next)
        // Add the new termName to the hashtable only after it's been fully constructed
        termHashtable(h) = termName
        termName
      }
    }
    if (synchronizeNames) nameLock.synchronized(body) else body
  }

  final def newTypeName(cs: Array[Char], offset: Int, len: Int, cachedString: String): TypeName =
    newTermName(cs, offset, len, cachedString).toTypeName

  /** Create a term name from string. */
  @deprecatedOverriding("To synchronize, use `override def synchronizeNames = true`", "2.11.0") // overridden in https://github.com/scala-ide/scala-ide/blob/master/org.scala-ide.sdt.core/src/scala/tools/eclipse/ScalaPresentationCompiler.scala
  def newTermName(s: String): TermName = newTermName(s.toCharArray(), 0, s.length(), null)

  /** Create a type name from string. */
  @deprecatedOverriding("To synchronize, use `override def synchronizeNames = true`", "2.11.0") // overridden in https://github.com/scala-ide/scala-ide/blob/master/org.scala-ide.sdt.core/src/scala/tools/eclipse/ScalaPresentationCompiler.scala
  def newTypeName(s: String): TypeName = newTermName(s).toTypeName

  /** Create a term name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
  final def newTermName(bs: Array[Byte], offset: Int, len: Int): TermName = {
    val chars = Codec.fromUTF8(bs, offset, len)
    newTermName(chars, 0, chars.length)
  }

  final def newTermNameCached(s: String): TermName =
    newTermName(s.toCharArray(), 0, s.length(), cachedString = s)

  final def newTypeNameCached(s: String): TypeName =
    newTypeName(s.toCharArray(), 0, s.length(), cachedString = s)

  /** Create a type name from the characters in cs[offset..offset+len-1]. */
  final def newTypeName(cs: Array[Char], offset: Int, len: Int): TypeName =
    newTermName(cs, offset, len, cachedString = null).toTypeName

  /** Create a type name from the UTF8 encoded bytes in bs[offset..offset+len-1]. */
  final def newTypeName(bs: Array[Byte], offset: Int, len: Int): TypeName =
    newTermName(bs, offset, len).toTypeName

  /**
   * Used by the GenBCode backend to lookup type names that are known to already exist. This method
   * might be invoked in a multi-threaded setting. Invoking newTypeName instead might be unsafe.
   *
   * can-multi-thread: names are added to the hash tables only after they are fully constructed.
   */
  final def lookupTypeName(cs: Array[Char]): TypeName = {
    val hash = hashValue(cs, 0, cs.length) & HASH_MASK
    var typeName = typeHashtable(hash)

    while ((typeName ne null) && (typeName.length != cs.length || !equals(typeName.start, cs, 0, cs.length))) {
      typeName = typeName.next
    }
    assert(typeName != null, s"TypeName ${new String(cs)} not yet created.")
    typeName
  }

// Classes ----------------------------------------------------------------------

  /** The name class.
   *  TODO - resolve schizophrenia regarding whether to treat Names as Strings
   *  or Strings as Names.  Give names the key functions the absence of which
   *  make people want Strings all the time.
   */
  sealed abstract class Name(protected val index: Int, protected val len: Int) extends NameApi {
    type ThisNameType >: Null <: Name
    protected[this] def thisName: ThisNameType

    // Note that "Name with ThisNameType" should be redundant
    // because ThisNameType <: Name, but due to SI-6161 the
    // compile loses track of this fact.

    /** Index into name table */
    def start: Int = index

    /** The next name in the same hash bucket. */
    def next: Name with ThisNameType

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
    def subName(from: Int, to: Int): Name with ThisNameType

    /** Return a new name of the same variety. */
    def newName(str: String): Name with ThisNameType

    /** Return a new name based on string transformation. */
    def mapName(f: String => String): Name with ThisNameType = newName(f(toString))

    /** Copy bytes of this name to buffer cs, starting at position `offset`. */
    final def copyChars(cs: Array[Char], offset: Int) =
      scala.compat.Platform.arraycopy(chrs, index, cs, offset, len)

    /** @return the ascii representation of this name */
    final def toChars: Array[Char] = {  // used by ide
      val cs = new Array[Char](len)
      copyChars(cs, 0)
      cs
    }

    /** @return the hash value of this name */
    final override def hashCode(): Int = index

    /** @return true if the string value of this name is equal
     *  to the string value of the given name or String.
     */
    def string_==(that: Name): Boolean   = (that ne null) && (toString == that.toString)
    def string_==(that: String): Boolean = (that ne null) && (toString == that)

    /****
     *  This has been quite useful to find places where people are comparing
     *  a TermName and a TypeName, or a Name and a String.

    override def equals(other: Any) = paranoidEquals(other)
    private def paranoidEquals(other: Any): Boolean = {
      val cmp = this eq other.asInstanceOf[AnyRef]
      if (cmp || !nameDebug)
        return cmp

      other match {
        case x: String  =>
          Console.println(s"Compared $debugString and String '$x'")
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
    ****/

    /** @return the i'th Char of this name */
    final def charAt(i: Int): Char = chrs(index + i)

    /** @return the index of first occurrence of char c in this name, length if not found */
    final def pos(c: Char): Int = pos(c, 0)

    /** @return the index of first occurrence of s in this name, length if not found */
    final def pos(s: String): Int = pos(s, 0)

    /** Returns the index of the first occurrence of character c in
     *  this name from start, length if not found.
     *
     *  @param c     the character
     *  @param start the index from which to search
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
     *  @param start the index from which to search
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

    /** Returns the index of the last occurrence of char c in this
     *  name from start, -1 if not found.
     *
     *  @param c     the character
     *  @param start the index from which to search
     *  @return      the index of the last occurrence of c
     */
    final def lastPos(c: Char, start: Int): Int = {
      var i = start
      while (i >= 0 && chrs(index + i) != c) i -= 1
      i
    }

    /** Does this name start with prefix? */
    final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

    /** Does this name start with prefix at given start index? */
    final def startsWith(prefix: Name, start: Int): Boolean = {
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == chrs(prefix.start + i))
        i += 1
      i == prefix.length
    }
    final def startsWith(prefix: String, start: Int): Boolean = {
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == prefix.charAt(i))
        i += 1
      i == prefix.length
    }

    /** Does this name end with suffix? */
    final def endsWith(suffix: Name): Boolean = endsWith(suffix, len)

    /** Does this name end with suffix just before given end index? */
    final def endsWith(suffix: Name, end: Int): Boolean = {
      var i = 1
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == chrs(suffix.start + suffix.length - i))
        i += 1
      i > suffix.length
    }
    final def endsWith(suffix: String, end: Int): Boolean = {
      var i = 1
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == suffix.charAt(suffix.length - i))
        i += 1
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
    final def startChar: Char                   = this charAt 0
    final def endChar: Char                     = this charAt len - 1
    final def startsWith(char: Char): Boolean   = len > 0 && startChar == char
    final def startsWith(name: String): Boolean = startsWith(name, 0)
    final def endsWith(char: Char): Boolean     = len > 0 && endChar == char
    final def endsWith(name: String): Boolean   = endsWith(name, len)

    /** Rewrite the confusing failure indication via result == length to
     *  the normal failure indication via result == -1.
     */
    private def fixIndexOf(idx: Int): Int = if (idx == length) -1 else idx

    def indexOf(ch: Char)                 = fixIndexOf(pos(ch))
    def indexOf(ch: Char, fromIndex: Int) = fixIndexOf(pos(ch, fromIndex))
    def indexOf(s: String)                = fixIndexOf(pos(s))

    /** The lastPos methods already return -1 on failure. */
    def lastIndexOf(ch: Char): Int  = lastPos(ch)
    def lastIndexOf(s: String): Int = toString lastIndexOf s

    /** Replace all occurrences of `from` by `to` in
     *  name; result is always a term name.
     */
    def replace(from: Char, to: Char): Name = {
      val cs = new Array[Char](len)
      var i = 0
      while (i < len) {
        val ch = charAt(i)
        cs(i) = if (ch == from) to else ch
        i += 1
      }
      newTermName(cs, 0, len)
    }

    /* TODO - reconcile/fix that encode returns a Name but
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
    def append(ch: Char)        = newName(toString + ch)
    def append(suffix: String)  = newName(toString + suffix)
    def append(suffix: Name)    = newName(toString + suffix)
    def append(separator: Char, suffix: Name) = newName(toString + separator + suffix)
    def prepend(prefix: String) = newName("" + prefix + this)

    def decodedName: ThisNameType = newName(decode)
    def isOperatorName: Boolean = decode != toString  // used by ide
    def longString: String      = nameKind + " " + decode
    def debugString = { val s = decode ; if (isTypeName) s + "!" else s }
  }

  implicit def AnyNameOps(name: Name): NameOps[Name]          = new NameOps(name)
  implicit def TermNameOps(name: TermName): NameOps[TermName] = new NameOps(name)
  implicit def TypeNameOps(name: TypeName): NameOps[TypeName] = new NameOps(name)

  /** FIXME: This is a good example of something which is pure "value class" but cannot
   *  reap the benefits because an (unused) $outer pointer so it is not single-field.
   */
  final class NameOps[T <: Name](name: T) {
    import NameTransformer._
    def stripSuffix(suffix: String): T = if (name endsWith suffix) dropRight(suffix.length) else name // OPT avoid creating a Name with `suffix`
    def stripSuffix(suffix: Name): T   = if (name endsWith suffix) dropRight(suffix.length) else name
    def take(n: Int): T                = name.subName(0, n).asInstanceOf[T]
    def drop(n: Int): T                = name.subName(n, name.length).asInstanceOf[T]
    def dropRight(n: Int): T           = name.subName(0, name.length - n).asInstanceOf[T]
    def dropLocal: TermName            = name.toTermName stripSuffix LOCAL_SUFFIX_STRING
    def dropSetter: TermName           = name.toTermName stripSuffix SETTER_SUFFIX_STRING
    def dropModule: T                  = this stripSuffix MODULE_SUFFIX_STRING
    def localName: TermName            = getterName append LOCAL_SUFFIX_STRING
    def setterName: TermName           = getterName append SETTER_SUFFIX_STRING
    def getterName: TermName           = dropTraitSetterSeparator.dropSetter.dropLocal

    private def dropTraitSetterSeparator: TermName =
      name indexOf TRAIT_SETTER_SEPARATOR_STRING match {
        case -1  => name.toTermName
        case idx => name.toTermName drop idx drop TRAIT_SETTER_SEPARATOR_STRING.length
      }
  }

  implicit val NameTag = ClassTag[Name](classOf[Name])

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
  private final class TermName_S(index0: Int, len0: Int, next0: TermName, override val toString: String) extends TermName(index0, len0, next0) {
    protected def createCompanionName(next: TypeName): TypeName = new TypeName_S(index, len, next, toString)
    override def newName(str: String): TermName = newTermNameCached(str)
  }
  private final class TypeName_S(index0: Int, len0: Int, next0: TypeName, override val toString: String) extends TypeName(index0, len0, next0) {
    override def newName(str: String): TypeName = newTypeNameCached(str)
  }

  private final class TermName_R(index0: Int, len0: Int, next0: TermName) extends TermName(index0, len0, next0) {
    protected def createCompanionName(next: TypeName): TypeName = new TypeName_R(index, len, next)
    override def toString = new String(chrs, index, len)
  }

  private final class TypeName_R(index0: Int, len0: Int, next0: TypeName) extends TypeName(index0, len0, next0) {
    override def toString = new String(chrs, index, len)
  }

  // SYNCNOTE: caller to constructor must synchronize if `synchronizeNames` is enabled
  sealed abstract class TermName(index0: Int, len0: Int, val next: TermName) extends Name(index0, len0) with TermNameApi {
    type ThisNameType = TermName
    protected[this] def thisName: TermName = this

    def isTermName: Boolean = true
    def isTypeName: Boolean = false
    def toTermName: TermName = this
    def toTypeName: TypeName = {
      def body = {
        // Re-computing the hash saves a field for storing it in the TermName
        val h = hashValue(chrs, index, len) & HASH_MASK
        var n = typeHashtable(h)
        while ((n ne null) && n.start != index)
          n = n.next

        if (n ne null) n
        else {
          val next = typeHashtable(h)
          val typeName = createCompanionName(next)
          // Add the new typeName to the hashtable only after it's been fully constructed
          typeHashtable(h) = typeName
          typeName
        }
      }
      if (synchronizeNames) nameLock.synchronized(body) else body
    }
    def newName(str: String): TermName = newTermName(str)
    def companionName: TypeName = toTypeName
    def subName(from: Int, to: Int): TermName =
      newTermName(chrs, start + from, to - from)

    def nameKind = "term"
    /** SYNCNOTE: caller must synchronize if `synchronizeNames` is enabled */
    protected def createCompanionName(next: TypeName): TypeName
  }

  implicit val TermNameTag = ClassTag[TermName](classOf[TermName])

  object TermName extends TermNameExtractor {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  sealed abstract class TypeName(index0: Int, len0: Int, val next: TypeName) extends Name(index0, len0) with TypeNameApi {
    type ThisNameType = TypeName
    protected[this] def thisName: TypeName = this

    def isTermName: Boolean = false
    def isTypeName: Boolean = true
    def toTermName: TermName = {
      def body = {
        // Re-computing the hash saves a field for storing it in the TypeName
        val h = hashValue(chrs, index, len) & HASH_MASK
        var n = termHashtable(h)
        while ((n ne null) && n.start != index)
          n = n.next

        assert (n ne null, s"TypeName $this is missing its correspondent")
        n
      }
      if (synchronizeNames) nameLock.synchronized(body) else body
    }
    def toTypeName: TypeName = this
    def newName(str: String): TypeName = newTypeName(str)
    def companionName: TermName = toTermName
    def subName(from: Int, to: Int): TypeName =
      newTypeName(chrs, start + from, to - from)

    def nameKind = "type"
    override def decode = if (nameDebug) super.decode + "!" else super.decode
  }

  implicit val TypeNameTag = ClassTag[TypeName](classOf[TypeName])

  object TypeName extends TypeNameExtractor {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
}
