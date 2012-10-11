package scala.reflect
package api

/** This trait defines Names (a Scala reflection concept) and operations on them.
 *
 *  Names are simple wrappers for strings. [[scala.reflect.api.Names#Name Name]] has two subtypes [[scala.reflect.api.Names#TermName TermName]] and [[scala.reflect.api.Names#TypeName TypeName]] which
 *  distinguish names of terms (like objects or members) and types. A term and a type of the
 *  same name can co-exist in an object. 
 *
 *  === Examples ===
 *
 *  To search for the `map` method (which is a term) declared in the `List` class,
 *  use `typeOf[List[_]].member(newTermName("map"))`. To search for a type member, use
 *  newTypeName instead.
 *
 *  See the [[docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]] for more about Scala Reflection.
 *
 *  @contentDiagram hideNodes "*Api"
 */
trait Names {
  /** An implicit conversion from String to TermName.
   *  Enables an alternative notation `"map": TermName` as opposed to `newTermName("map")`.
   *  @group Names
   */
  implicit def stringToTermName(s: String): TermName = newTermName(s)

  /** An implicit conversion from String to TypeName.
   *  Enables an alternative notation `"List": TypeName` as opposed to `newTypeName("List")`.
   *  @group Names
   */
  implicit def stringToTypeName(s: String): TypeName = newTypeName(s)

  /** The abstract type of names.
   *  @group Names
   */
  type Name >: Null <: NameApi

  /** A tag that preserves the identity of the `Name` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val NameTag: ClassTag[Name]

  /** The abstract type of names representing terms.
   *  @group Names
   */
  type TypeName >: Null <: Name

  /** A tag that preserves the identity of the `TypeName` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
implicit val TypeNameTag: ClassTag[TypeName]

  /** The abstract type of names representing types.
   *  @group Names
   */
  type TermName >: Null <: Name

  /** A tag that preserves the identity of the `TermName` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val TermNameTag: ClassTag[TermName]

  /** The API of Name instances.
   *  @group API
   */
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
   *  @group Names
   */
  def newTermName(s: String): TermName

  /** Creates a new type name.
   *  @group Names
   */
  def newTypeName(s: String): TypeName
}
