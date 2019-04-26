/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

import java.util.concurrent.ConcurrentHashMap

import scala.reflect.api.NameTableApi

trait Names extends api.Names {
  type NameTable = scala.reflect.internal.NameTable
  override val nameTable: NameTable = newNameTable

  protected def newNameTable: NameTable = new NameTable

  def nameTableSize: Int = nameTable.nameTableSize
  override final type Name = nameTable.AName
  override final type TypeName = nameTable.TypeNameImpl
  override final type TermName = nameTable.TermNameImpl

  implicit final val NameTag = ClassTag[Name](classOf[Name])
  implicit final val TermNameTag = ClassTag[TermName](classOf[TermName])
  implicit final val TypeNameTag = ClassTag[TypeName](classOf[TypeName])

  override final def newTypeName(value: String): TypeName = nameTable.newTypeName(value)
  override final def newTermName(value: String): TermName = nameTable.newTermName(value)
  object TermName extends TermNameExtractor {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }
  object TypeName extends TypeNameExtractor {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
  //deprecated stuff
  @deprecated
  @inline final def newTermNameCached(s: String): TermName = newTermName(s)

  @deprecated
  @inline final def newTypeNameCached(s: String): TypeName = newTypeName(s)
}
class NameTable extends NameTableApi {
  override final type Name = AName
  override final type TypeName = TypeNameImpl
  override final type TermName = TermNameImpl

  def nameTableSize: Int = cache.size

  private[this] final val cache = new ConcurrentHashMap[String, TermNameImpl](1000, 0.75F, 1)

  override final def newTermName(value: String): TermNameImpl = {
    //TODO consider a better structure to use than a CHM
    var res = cache.get(value)
    if (res eq null) {
      val next = new TermNameImpl(value)
      res = cache.putIfAbsent(value, next)
      if (res eq null)
        res = next
    }
    res
    //same as cache.computeIfAbsent(value, new NameHolder(_)) but faster
  }
  final def newTypeName(value:String): TypeName = newTermName(value).companionName

  abstract sealed class AName extends NameApi with CharSequence {
    type ThisNameType <: AName
    @inline override final def length(): Int = value.length
    override final def subSequence(start: Int, end: Int): CharSequence = value.subSequence(start, end)
    override def decoded: String = decodedName.toString
    override def encoded: String = encodedName.toString
    @inline private[NameTable] def rawString = value
    def decodedName: ThisNameType
    def encodedName: ThisNameType

    //non API methods
    protected def value: String

    def companionName: AName

    /** Return the subname with characters from from to to-1. */
    def subName(from: Int, to: Int): ThisNameType =
      if (from == 0 && to == value.length) thisNameType
      else if (from < 0 || to > value.length)
        throw new IllegalArgumentException(s" from:$from to:$to is illegal for length$length")
      else newName(value.substring(from, Math.max(to, from)))
    private def thisNameType = this.asInstanceOf[ThisNameType]

    /** Return a new name of the same variety. */
    def newName(str: String): ThisNameType

    /** Return a new name based on string transformation. */
    def mapName(f: String => String): ThisNameType = newName(f(toString))

    /** Copy bytes of this name to buffer cs, starting at position `offset`. */
    final def copyChars(cs: Array[Char], offset: Int) =
      value.copyToArray(cs,offset)

    def nameKind: String

    def append(ch: Char)        = newName(toString + ch)
    def append(suffix: String)  = if (suffix.length == 0) thisNameType else newName(toString + suffix)
    def append(suffix: Name)    = if (suffix.length == 0) thisNameType else newName(toString + suffix)
    def append(separator: Char, suffix: Name) = newName(toString + separator + suffix)
    def prepend(prefix: String) = newName(prefix + value)

    def isOperatorName: Boolean = decodedName != this  // used by ide
    def longString: String      = nameKind + " " + decoded
    def debugString :String

    //avoid the implicit NameOps

    import NameTransformer._
    def stripSuffix(suffix: String): ThisNameType = if (value endsWith suffix) dropRight(suffix.length) else thisNameType // OPT avoid creating a Name with `suffix`
    def stripSuffix(suffix: Name): ThisNameType   = if (value endsWith suffix.toString) dropRight(suffix.length) else thisNameType
    def take(n: Int): ThisNameType                = subName(0, n)
    def drop(n: Int): ThisNameType                = subName(n, value.length)
    def dropRight(n: Int): ThisNameType           = subName(0, value.length - n)
    def dropLocal: TermName                       = toTermName stripSuffix LOCAL_SUFFIX_STRING
    def dropSetter: TermName                      = toTermName stripSuffix SETTER_SUFFIX_STRING
    def dropModule: ThisNameType                  = stripSuffix(MODULE_SUFFIX_STRING)
    def localName: TermName                       = newTermName(getterNameString + LOCAL_SUFFIX_STRING)
    def setterName: TermName                      = newTermName(getterNameString + SETTER_SUFFIX_STRING)
    def getterName: TermName                      = {val rawString = getterNameString; if (rawString == value) toTermName else newTermName(rawString)}

    private def getterNameString: String          = unSuffix(unSuffix(dropTraitSetterSeparator,SETTER_SUFFIX_STRING), LOCAL_SUFFIX_STRING)
    private def unSuffix(value: String, suffix: String)  = if (value endsWith suffix) value.substring(0, value.length - suffix.length) else value

    private def dropTraitSetterSeparator: String =
      value indexOf TRAIT_SETTER_SEPARATOR_STRING match {
        case -1  => value
        case idx => value.substring(idx + TRAIT_SETTER_SEPARATOR_STRING.length)
      }

    /** @return the i'th Char of this name */
    @inline final def charAt(i: Int): Char = value.charAt(i)

    //deprcated stuff maybe
    @deprecated @inline final def decode = decoded
    @deprecated @inline final def encode = encodedName
    @deprecated @inline final def nonEmpty = value.length != 0
    @deprecated @inline final def isEmpty = value.length == 0
    /** @return the ascii representation of this name */
    @deprecated final def toChars: Array[Char] = {  // used by ide
      value.toCharArray
    }
    //basic string like operations
    /** @return true if the string value of this name is equal
      *  to the string value of the given name or String.
      */
    @deprecated def string_==(that: Name): Boolean   = (that ne null) && (value == that.toString)
    //value == that ??
    @deprecated def string_==(that: String): Boolean = (that ne null) && (value == that)


    //      /********************  IS POS REALLY USED **********/
//      /** @return the index of first occurrence of char c in this name, length if not found */
//      final def pos(c: Char): Int = pos(c, 0)
//
//      /** @return the index of first occurrence of s in this name, length if not found */
//      final def pos(s: String): Int = pos(s, 0)
//
//      /** Returns the index of the first occurrence of character c in
//        *  this name from start, length if not found.
//        *
//        *  @param c     the character
//        *  @param start the index from which to search
//        *  @return      the index of the first occurrence of c
//        */
//      final def pos(c: Char, start: Int): Int = {
//        val index = value.indexOf(c,start)
//        if (index == -1) length else index
//      }
//
//      /** Returns the index of the first occurrence of nonempty string s
//        *  in this name from start, length if not found.
//        *
//        *  @param s     the string
//        *  @param start the index from which to search
//        *  @return      the index of the first occurrence of s
//        */
//      final def pos(s: String, start: Int): Int = {
//        val index = value.indexOf(s,start)
//        if (index == -1) length else index
//      }
//
//      /** Returns the index of last occurrence of char c in this
//        *  name, -1 if not found.
//        *
//        *  @param c the character
//        *  @return  the index of the last occurrence of c
//        */
//      final def lastPos(c: Char): Int = lastPos(c, length - 1)
//
//      /** Returns the index of the last occurrence of char c in this
//        *  name from start, -1 if not found.
//        *
//        *  @param c     the character
//        *  @param start the index from which to search
//        *  @return      the index of the last occurrence of c
//        */
//      final def lastPos(c: Char, start: Int): Int = {
//        value.lastIndexOf(c,start)
//     }
//      /********************  IS POS REALLY USED **********/

    /** Does this name start with prefix? */
    @inline final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

    /** Does this name start with prefix at given start index? */
    @inline final def startsWith(prefix: Name, start: Int): Boolean = {
      startsWith(prefix.toString, start)
    }
    @inline final def startsWith(prefix: String, start: Int): Boolean = {
      value.startsWith(prefix, start)
    }

    /** Does this name end with suffix? */
    @inline final def endsWith(suffix: Name): Boolean = endsWith(suffix, length)

    /** Does this name end with suffix just before given end index? */
    @inline final def endsWith(suffix: Name, end: Int): Boolean = {
      endsWith(suffix.toString, end)
    }
    @inline final def endsWith(suffix: String, end: Int): Boolean = {
      value.startsWith(suffix, end - suffix.length)
    }

    @inline final def containsName(subname: String): Boolean = value contains subname
    @inline final def containsName(subname: Name): Boolean = containsName(subname.toString)

    final def containsChar(ch: Char): Boolean = value contains ch
    /** Some thoroughly self-explanatory convenience functions.  They
      *  assume that what they're being asked to do is known to be valid.
      */
    @inline final def startChar: Char                   = value charAt 0
    @inline final def endChar: Char                     = value charAt length - 1
    @inline final def startsWith(char: Char): Boolean   = length > 0 && startChar == char
    @inline final def startsWith(name: String): Boolean = startsWith(name, 0)
    @inline final def endsWith(char: Char): Boolean     = length > 0 && endChar == char
    @inline final def endsWith(name: String): Boolean   = endsWith(name, length)


    @inline final def indexOf(ch: Char)                 = value.indexOf(ch)
    @inline final def indexOf(ch: Char, fromIndex: Int) = value.indexOf(ch, fromIndex)
    @inline final def indexOf(s: String)                = value.indexOf(s)

    @inline final def lastIndexOf(ch: Char): Int  = value lastIndexOf ch
    @inline final def lastIndexOf(s: String): Int = value lastIndexOf s

    /** Replace all occurrences of `from` by `to` in
      *  name; result is always a term name.
      */
    def replace(from: Char, to: Char): Name = {
      val replaced = value.replace(from, to)
      if (replaced eq value) this else newTermName(replaced)
    }
  }
  final class TermNameImpl(override val toString: String) extends AName with TermNameApi {
    type ThisNameType = TermName

    override def isTermName = true
    override def isTypeName = false

    override protected def value: String = toString

    override def toTermName: TermName = this
    override def toTypeName: TypeName = typeName

    override def companionName = typeName
    override def newName(str: String) = newTermName(str)
    override def nameKind = "term"

    private var identifier_ : Short = _
    def isJavaIdentifier = (identifier_ & 0x1000) != 0
    def isScalaIdentifier = (identifier_ & 0x2000) != 0
    def identifier = identifier_ & 0x80FF
    def markAsIdentifier(java: Boolean, newIdentifier: Int) {
      require((identifier.toShort & 0x80FF) == identifier.toShort)
      val flag = (if (java) 0x1000 else 0x2000).toShort
      if (identifier_ == 0) {
        //first call
        this.identifier_ = (newIdentifier | flag).toShort
      } else {
        require(identifier == newIdentifier)
        this.identifier_ = (this.identifier_ | flag).toShort
      }
    }

    def debugString = decoded

    override lazy final val decodedName: TermName = {
      if (value contains '$') {
        val res = NameTransformer.decode(value)
        if (res == value) this
        else newName(res)
      }
      else this
    }
    override lazy final val encodedName: TermName = {
      val res = NameTransformer.encode(value)
      if (res == value) this else newName(res)
    }

    private[NameTable] lazy val typeName: TypeNameImpl = new TypeNameImpl(this)

  }
  final class TypeNameImpl(term: TermNameImpl) extends AName with TypeNameApi {
    type ThisNameType = TypeName
    override def isTermName = false
    override def isTypeName = true
    override def decodedName: TypeName = term.decodedName.typeName
    override def encodedName: TypeName  = term.encodedName.typeName


    override protected def value: String = term.toString
    override def toString: String = term.toString

    override def toTermName: TermName = term
    override def toTypeName: TypeName = this

    override def companionName = term
    override def newName(str: String) = newTypeName(str)
    override def nameKind = "type"
    def debugString = decoded + "!"
  }
}
