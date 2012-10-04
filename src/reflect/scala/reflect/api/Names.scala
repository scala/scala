package scala.reflect
package api

/** A slice of [[scala.reflect.api.Universe the Scala reflection cake]] that defines names and operations on them.
 *  See [[scala.reflect.api.Universe]] for a description of how the reflection API is encoded with the cake pattern.
 *
 *  Scala has separate namespaces for term names and type names. For example it is possible to have
 *  a class named `C` and an object named `C` declared in the same lexical scope.
 *
 *  Therefore the Scala reflection API models names using strongly-typed objects rather than strings:
 *  [[scala.reflect.api.Names#TermName]] and [[scala.reflect.api.Names#TypeName]].
 *
 *  A Name wraps a string as the name for either a type ([[TypeName]]) of a term ([[TermName]]).
 *  Two names are equal, if the wrapped string are equal and they are either both `TypeName` or both `TermName`.
 *  The same string can co-exist as a `TypeName` and a `TermName`, but they would not be equal.
 *  Names are interned. That is, for two names `name1` and `name2`, `name1 == name2` implies `name1 eq name2`.
 *  Name instances also can perform mangling and unmangling of symbolic names.
 *
 *  === Examples ===
 *
 *  To search for the `map` method declared in the `List` class, one uses
 *  `typeOf[List[_]].member(newTermName("map"))` to explicitly specify that a term is looked up.
 *
 *  An alternative notation makes use of implicit conversions from `String` to `TermName` and `TypeName`:
 *  `typeOf[List[_]].member("map": TermName)`. Note that there's no implicit conversion from `String` to `Name`,
 *  because it would be unclear whether such a conversion should produce a term name or a type name.
 *
 *  Finally some names that bear special meaning for the compiler are defined in [[scala.reflect.api.StandardNames]].
 *  For example, `WILDCARD` represents `_` and `CONSTRUCTOR` represents the standard JVM name for constructors, `<init>`.
 *  Prefer using such constants instead of spelling the names out explicitly.
 */
trait Names {
  /** An implicit conversion from String to TermName.
   *  Enables an alternative notation `"map": TermName` as opposed to `newTermName("map")`.
   */
  implicit def stringToTermName(s: String): TermName = newTermName(s)

  /** An implicit conversion from String to TypeName.
   *  Enables an alternative notation `"List": TypeName` as opposed to `newTypeName("List")`.
   */
  implicit def stringToTypeName(s: String): TypeName = newTypeName(s)

  /** The abstract type of names. */
  type Name >: Null <: NameApi

  /** A tag that preserves the identity of the `Name` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val NameTag: ClassTag[Name]

  /** The abstract type of names representing terms. */
  type TypeName >: Null <: Name

  /** A tag that preserves the identity of the `TypeName` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeNameTag: ClassTag[TypeName]

  /** The abstract type of names representing types. */
  type TermName >: Null <: Name

  /** A tag that preserves the identity of the `TermName` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TermNameTag: ClassTag[TermName]

  /** The API of Name instances. */
  abstract class NameApi {
    /** Checks wether the name is a a term name */
    def isTermName: Boolean

    /** Checks wether the name is a a type name */
    def isTypeName: Boolean

    /** Returns a term name that wraps the same string as `this` */
    def toTermName: TermName

    /** Returns a type name that wraps the same string as `this` */
    def toTypeName: TypeName

    /** Replaces all occurrences of \$op_names in this name by corresponding operator symbols.
     *  Example: `foo_\$plus\$eq` becomes `foo_+=`
     */
    def decoded: String

    /** Replaces all occurrences of operator symbols in this name by corresponding \$op_names.
     *  Example: `foo_+=` becomes `foo_\$plus\$eq`.
     */
    def encoded: String

    /** The decoded name, still represented as a name.
     */
    def decodedName: Name

    /** The encoded name, still represented as a name.
     */
    def encodedName: Name
  }

  /** Create a new term name.
   */
  def newTermName(s: String): TermName

  /** Creates a new type name.
   */
  def newTypeName(s: String): TypeName
}
