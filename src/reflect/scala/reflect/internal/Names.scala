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

import scala.annotation.tailrec

trait Names extends api.Names {
  def nameTableSize: Int = cache.size

  override final type Name = NameHolder#AName
  override final type TypeName = NameHolder#TypeNameImpl
  override final type TermName = NameHolder#TermNameImpl

  implicit val NameTag = ClassTag[Name](classOf[Name])
  implicit val TermNameTag = ClassTag[TermName](classOf[TermName])
  implicit val TypeNameTag = ClassTag[TypeName](classOf[TypeName])


  override final def newTermName(s: String): TermName = newName(s).termName
  override final def newTypeName(s: String): TypeName = newName(s).typeName

  override object TermName extends TermNameExtractor {
    @inline override def apply(s: String): TermName = newTermName(s)
    override def unapply(name: TermName): Option[String] = Some(name.rawString)
  }
  override object TypeName extends TypeNameExtractor {
    @inline override def apply(s: String): TypeName = newTypeName(s)
    override def unapply(name: TypeName): Option[String] = Some(name.rawString)
  }
  //TODO consider a better structure to use than a CHM
  final def newName(value:String) = {
    var res = cache.get(value)
    if (res eq null) {
      val next = new NameHolder(value)
      res = cache.putIfAbsent(value, next)
      if (res eq null)
        res = next
    }
    res
    //same as cache.computeIfAbsent(value, new NameHolder(_)) but faster
  }
  private[this] final val cache = new ConcurrentHashMap[String, NameHolder](1000, 0.75F, 1)
  //deprecated stuff
  @deprecated @inline final def newTermNameCached(s: String): TermName = newTermName(s)
  @deprecated @inline final def newTypeNameCached(s: String): TypeName = newTypeName(s)

  final class NameHolder(private[NameHolder] val value: String) {

   lazy val decodedHolder: NameHolder = {
      if (value contains '$') {
        val res = NameTransformer.decode(value)
        if (res == value) this
        else newName(res)
      }
      else this
    }
    lazy val encodedHolder: NameHolder = {
      val res = NameTransformer.encode(value)
      if (res == value) this else newName(res)
    }

    final val termName:TermName = new TermNameImpl
    final val typeName:TypeName = new TypeNameImpl

    abstract class AName extends NameApi with CharSequence {
      type ThisNameType <: NameHolder#AName
      @inline override final def length(): Int = value.length
      override final def subSequence(start: Int, end: Int): CharSequence = value.subSequence(start, end)
      override def decoded: String = decodedName.raw
      override def encoded: String = encodedName.raw
      @inline private[Names] def rawString = value
      override def decodedName: ThisNameType
      override def encodedName: ThisNameType

      //non API methods

      def companionName: Name

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

      /** @return the ascii representation of this name */
      final def toChars: Array[Char] = {  // used by ide
        value.toCharArray
      }
      def nameKind: String
      private[Names] def raw = value
      override def toString = value

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
      def stripSuffix(suffix: Name): ThisNameType   = if (value endsWith suffix.raw) dropRight(suffix.length) else thisNameType
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

      //basic string like operations
      /** @return true if the string value of this name is equal
        *  to the string value of the given name or String.
        */
      def string_==(that: Name): Boolean   = (that ne null) && (value == that.raw)
      def string_==(that: String): Boolean = (that ne null) && (value == that)

      /** @return the i'th Char of this name */
      @inline final def charAt(i: Int): Char = value.charAt(i)

      //deprcated stuff maybe
      @deprecated @inline final def decode = decoded
      @deprecated @inline final def encode = encodedName
      @deprecated @inline final def nonEmpty = value.length != 0
      @deprecated @inline final def isEmpty = value.length == 0
      //@deprecated @inline final def start = hashCode()


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
      final def startsWith(prefix: Name): Boolean = startsWith(prefix, 0)

      /** Does this name start with prefix at given start index? */
      final def startsWith(prefix: Name, start: Int): Boolean = {
        startsWith(prefix.raw, start)
      }
      final def startsWith(prefix: String, start: Int): Boolean = {
        value.startsWith(prefix, start)
      }

      /** Does this name end with suffix? */
      final def endsWith(suffix: Name): Boolean = endsWith(suffix, length)

      /** Does this name end with suffix just before given end index? */
      final def endsWith(suffix: Name, end: Int): Boolean = {
        endsWith(suffix.raw, end)
      }
      final def endsWith(suffix: String, end: Int): Boolean = {
        value.startsWith(suffix, end - suffix.length)
      }

      final def containsName(subname: String): Boolean = value contains subname
      final def containsName(subname: Name): Boolean = containsName(subname.raw)

      final def containsChar(ch: Char): Boolean = value contains ch
      /** Some thoroughly self-explanatory convenience functions.  They
        *  assume that what they're being asked to do is known to be valid.
        */
      final def startChar: Char                   = value charAt 0
      final def endChar: Char                     = value charAt length - 1
      final def startsWith(char: Char): Boolean   = length > 0 && startChar == char
      final def startsWith(name: String): Boolean = startsWith(name, 0)
      final def endsWith(char: Char): Boolean     = length > 0 && endChar == char
      final def endsWith(name: String): Boolean   = endsWith(name, length)


      def indexOf(ch: Char)                 = value.indexOf(ch)
      def indexOf(ch: Char, fromIndex: Int) = value.indexOf(ch, fromIndex)
      def indexOf(s: String)                = value.indexOf(s)

      def lastIndexOf(ch: Char): Int  = value lastIndexOf ch
      def lastIndexOf(s: String): Int = value lastIndexOf s

      /** Replace all occurrences of `from` by `to` in
        *  name; result is always a term name.
        */
      def replace(from: Char, to: Char): Name = {
        val replaced = value.replace(from, to)
        if (replaced eq value) this else newTermName(replaced)
      }
    }
    final class TermNameImpl extends AName with TermNameApi {
      type ThisNameType = TermName
      override def isTermName = true
      override def isTypeName = false
      override def decodedName: TermName = decodedHolder.termName
      override def encodedName: TermName = encodedHolder.termName

      override def toTermName: TermName = this
      override def toTypeName: TypeName = typeName

      override def companionName = toTypeName
      override def newName(str: String) = newTermName(str)
      override def nameKind = "term"

      private var identifier_ : Short = _
      def isJavaIdentifier = (identifier_ & 0x1000) != 0
      def isScalaIdentifier = (identifier_ & 0x2000) != 0
      def identifier = identifier_ & 0x80FF
      def markAsIdentifier(java: Boolean, newIdentifier: Int) {
        require((identifier.toShort & 0x80FF) == identifier.toShort)
        val flag =  (if (java) 0x1000 else 0x2000).toShort
        if (identifier_ == 0) {
          //first call
          this.identifier_ = (newIdentifier | flag).toShort
        } else {
          require(identifier == newIdentifier)
          this.identifier_ = (this.identifier_ | flag).toShort
        }
      }

      def debugString = decoded
    }
    final class TypeNameImpl extends AName with TypeNameApi {
      type ThisNameType = TypeName
      override def isTermName = false
      override def isTypeName = true
      override def decodedName: TypeName = decodedHolder.typeName
      override def encodedName: TypeName  = encodedHolder.typeName

      override def toTermName: TermName = termName
      override def toTypeName: TypeName = this

      override def companionName = termName
      override def newName(str: String) = newTypeName(str)
      override def nameKind = "type"
      def debugString = decoded + "!"
    }

    override def hashCode = value.hashCode
    override def equals(other: Any) = other.asInstanceOf[AnyRef] eq this
  }
}
