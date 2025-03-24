/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

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
  private[this] val nameLock: Object = new Object


  /** Memory to store all names sequentially. */
  private[this] var _chrs: Array[Char] = new Array[Char](NAME_SIZE) // TODO this ought to be private
  @deprecated("Don't access name table contents directly.", "2.12.9")
  def chrs: Array[Char] = _chrs
  @deprecated("Don't access name table contents directly.", "2.12.9")
  def chrs_=(cs: Array[Char]) = _chrs = cs
  private[this] var nc = 0
  final def nameTableSize: Int = nc

  /** Hashtable for finding term names quickly. */
  private[this] val termHashtable = new Array[TermName](HASH_SIZE)

  /** Hashtable for finding type names quickly. */
  private[this] val typeHashtable = new Array[TypeName](HASH_SIZE)

  final def allNames(): Iterator[TermName] = termHashtable.iterator.filter(_ ne null).flatMap(n => Iterator.iterate(n)(_.next).takeWhile(_ ne null))

  private def hashValue(cs: Array[Char], offset: Int, len: Int): Int = {
    var h = 0
    var i = 0
    while (i < len) {
      h = 31 * h + cs(i + offset)
      i += 1
    }
    h
  }

  /** Is (the ASCII representation of) name at given index equal to
   *  cs[offset..offset+len-1]?
   */
  private def equals(index: Int, cs: Array[Char], offset: Int, len: Int): Boolean = {
    var i = 0
    val chrs = _chrs
    while ((i < len) && (chrs(index + i) == cs(offset + i)))
      i += 1
    i == len
  }

  /** Enter characters into chrs array. */
  private def enterChars(cs: Array[Char], offset: Int, len: Int): Unit = {
    var i = 0
    while (i < len) {
      if (nc + i == _chrs.length) {
        val newchrs = new Array[Char](_chrs.length * 2)
        java.lang.System.arraycopy(_chrs, 0, newchrs, 0, _chrs.length)
        _chrs = newchrs
      }
      _chrs(nc + i) = cs(offset + i)
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
   *  (e.g. if a name has the string "\$class" in it, then fail if that
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
        if (cs == _chrs) {
          // Optimize for subName, the new name is already stored in chrs
          startIndex = offset
        } else {
          startIndex = nc
          enterChars(cs, offset, len)
        }
        val next = termHashtable(h)
        val termName = new TermName(startIndex, len, next, cachedString)
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

  // Dummy trait to make Name#isEmpty with override keyword at JDK before 15
  sealed trait NameHasIsEmpty {
    def isEmpty: Boolean
  }

  /** The name class.
   *  TODO - resolve schizophrenia regarding whether to treat Names as Strings
   *  or Strings as Names.  Give names the key functions the absence of which
   *  make people want Strings all the time.
   */
  sealed abstract class Name(protected val index: Int, protected val len: Int, protected val cachedString: String) extends NameApi with NameHasIsEmpty with CharSequence {
    type ThisNameType >: Null <: Name
    protected[this] def thisName: ThisNameType

    /** Index into name table */
    final def start: Int = index

    /** The next name in the same hash bucket. */
    def next: ThisNameType

    /** The length of this name. */
    final def length: Int = len
    final def nonEmpty = !isEmpty
    // This method implements NameHasIsEmpty, and overrides CharSequence's isEmpty on JDK 15+
    override final def isEmpty = length == 0

    def nameKind: String
    def isTermName: Boolean
    def isTypeName: Boolean
    def toTermName: TermName
    def toTypeName: TypeName
    def companionName: Name

    final def asTypeOf[N <: Name](other: N): N =
      (if (other.isTermName) toTermName else toTypeName).asInstanceOf[N]

    /** Return the subname with characters from from to to-1. */
    def subName(from: Int, to: Int): ThisNameType

    override def subSequence(from: Int, to: Int): CharSequence = subName(from, to)

    /** Return a new name of the same variety. */
    def newName(str: String): ThisNameType

    /** Return a new name based on string transformation. */
    def mapName(f: String => String): ThisNameType = newName(f(toString))

    /** Copy bytes of this name to buffer cs, starting at position `offset`. */
    final def copyChars(cs: Array[Char], offset: Int) =
      java.lang.System.arraycopy(_chrs, index, cs, offset, len)

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
    final def charAt(i: Int): Char = _chrs(index + i)

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
      val chrs = _chrs
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
      val sLen = s.length()
      if (sLen == 1) return i
      val chrs = _chrs
      while (i + sLen <= len) {
        var j = 1
        while (s.charAt(j) == chrs(index + i + j)) {
          j += 1
          if (j == sLen) return i
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
      val chrs = _chrs
      while (i >= 0 && chrs(index + i) != c) i -= 1
      i
    }

    /** Does this name start with prefix? */
    final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

    /** Does this name start with prefix at given start index? */
    final def startsWith(prefix: Name, start: Int): Boolean = {
      val chrs = _chrs
      var i = 0
      while (i < prefix.length && start + i < len &&
             chrs(index + start + i) == chrs(prefix.start + i))
        i += 1
      i == prefix.length
    }
    final def startsWith(prefix: String, start: Int): Boolean = {
      val chrs = _chrs
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
      val chrs = _chrs
      while (i <= suffix.length && i <= end &&
             chrs(index + end - i) == chrs(suffix.start + suffix.length - i))
        i += 1
      i > suffix.length
    }
    final def endsWith(suffix: String, end: Int): Boolean = {
      var i = 1
      val chrs = _chrs
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
      val chrs = _chrs
      while (i < max) {
        if (chrs(i) == ch)
          return true
        i += 1
      }
      false
    }

    def lastIndexOf(s: String): Int = if (s.isEmpty) length else {
      val slength   = s.length()
      val lastIndex = slength - 1
      val lastChar  = s.charAt(lastIndex)
      val contents  = _chrs
      val base      = start
      val min       = base + lastIndex
      var end       = base + length - 1

      while (end >= min) {
        if (contents(end) == lastChar) {
          var i  = end - 1
          val i0 = i - lastIndex
          var at = lastIndex - 1
          while (i > i0 && contents(i) == s.charAt(at)) {
            i -= 1
            at -= 1
          }
          if (i == i0) return i0 + 1 - base
        }
        end -= 1
      }
      -1
    }

    /** Some thoroughly self-explanatory convenience functions.  They
     *  assume that what they're being asked to do is known to be valid.
     */
    final def startChar: Char                   = charAt(0)
    final def endChar: Char                     = charAt(len - 1)
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

    def stripSuffix(suffix: String): ThisNameType =
      if (endsWith(suffix)) dropRight(suffix.length) else thisName // OPT avoid creating a Name with `suffix`

    def stripSuffix(suffix: Name): ThisNameType =
      if (endsWith(suffix)) dropRight(suffix.length) else thisName

    def take(n: Int): ThisNameType      = subName(0, n)
    def drop(n: Int): ThisNameType      = subName(n, length)
    def dropRight(n: Int): ThisNameType = subName(0, length - n)

    def dropLocal: TermName      = toTermName stripSuffix NameTransformer.LOCAL_SUFFIX_STRING
    def dropSetter: TermName     = toTermName stripSuffix NameTransformer.SETTER_SUFFIX_STRING
    def dropModule: ThisNameType = stripSuffix(NameTransformer.MODULE_SUFFIX_STRING)
    def localName: TermName      = getterName append NameTransformer.LOCAL_SUFFIX_STRING
    def setterName: TermName     = getterName append NameTransformer.SETTER_SUFFIX_STRING
    def getterName: TermName     = dropTraitSetterSeparator.dropSetter.dropLocal
    def extensionName: TermName  = append("$extension").toTermName

    private def dropTraitSetterSeparator: TermName =
      indexOf(NameTransformer.TRAIT_SETTER_SEPARATOR_STRING) match {
        case -1  => toTermName
        case idx => toTermName.drop(idx + NameTransformer.TRAIT_SETTER_SEPARATOR_STRING.length)
      }

    def decodedName: ThisNameType = newName(decode)
    def isOperatorName: Boolean   = decoded != toString
    def longString: String        = s"$nameKind $decoded"
    def debugString               = if (isTypeName) s"$decoded!" else decoded

    final def toStringWithSuffix(suffix: String): String = {
      val builder = new java.lang.StringBuilder(length + suffix.length)
      builder.append(this: CharSequence)
      builder.append(suffix)
      builder.toString
    }

    override final def toString: String = if (cachedString == null) new String(_chrs, index, len) else cachedString
    final def appendTo(buffer: java.lang.StringBuffer, start: Int, length: Int): Unit =
      buffer.append(_chrs, this.start + start, length)
    final def appendTo(sb: StringBuilder, start: Int, length: Int): sb.type =
      sb.appendAll(_chrs, this.start + start, length)
  }

  implicit val NameTag: ClassTag[Name] = ClassTag[Name](classOf[Name])

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


  // SYNCNOTE: caller to constructor must synchronize if `synchronizeNames` is enabled
  final class TermName(index0: Int, len0: Int, val next: TermName, cachedString: String) extends Name(index0, len0, cachedString) with TermNameApi {
    type ThisNameType = TermName
    protected[this] def thisName: TermName = this

    def isTermName: Boolean = true
    def isTypeName: Boolean = false
    def toTermName: TermName = this
    def toTypeName: TypeName = {
      def body = {
        // Re-computing the hash saves a field for storing it in the TermName
        val h = hashValue(_chrs, index, len) & HASH_MASK
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
      newTermName(_chrs, start + from, to - from)

    def nameKind = "term"
    private def createCompanionName(next: TypeName): TypeName = new TypeName(index, len, next, cachedString)
  }

  implicit val TermNameTag: ClassTag[TermName] = ClassTag[TermName](classOf[TermName])

  object TermName extends TermNameExtractor {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  final class TypeName(index0: Int, len0: Int, val next: TypeName, cachedString: String) extends Name(index0, len0, cachedString) with TypeNameApi {
    type ThisNameType = TypeName
    protected[this] def thisName: TypeName = this

    def isTermName: Boolean = false
    def isTypeName: Boolean = true
    def toTermName: TermName = {
      def body = {
        // Re-computing the hash saves a field for storing it in the TypeName
        val h = hashValue(_chrs, index, len) & HASH_MASK
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
      newTypeName(_chrs, start + from, to - from)

    def nameKind = "type"
    override def decode = if (nameDebug) super.decode + "!" else super.decode
  }

  implicit val TypeNameTag: ClassTag[TypeName] = ClassTag[TypeName](classOf[TypeName])

  object TypeName extends TypeNameExtractor {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
}
