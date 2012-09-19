package scala.reflect
package api

/** A trait that manages names.
 *
 *  @see TermName
 *  @see TypeName
 */
trait Names {
  // Intentionally no implicit from String => Name.
  implicit def stringToTermName(s: String): TermName = newTermName(s)
  implicit def stringToTypeName(s: String): TypeName = newTypeName(s)

  /**
   * The abstract type of names
   *
   * A Name wraps a string as the name for either a type ([[TypeName]]) of a term ([[TermName]]).
   * Two names are equal, if the wrapped string are equal and they are either both `TypeName` or both `TermName`.
   * The same string can co-exist as a `TypeName` and a `TermName`, but they would not be equal.
   * Names are interned. That is, for two names `name11 and `name2`,
   *  `name1 == name2` implies `name1 eq name2`.
   *
   *  One of the reasons for the existence of names rather than plain strings is being more explicit about what is a name and if it represents a type or a term.
   */
  type Name >: Null <: NameApi
  implicit val NameTag: ClassTag[Name]

  /** The abstract type of names representing terms */
  type TypeName >: Null <: Name
  implicit val TypeNameTag: ClassTag[TypeName]

  /** The abstract type of names representing types */
  type TermName >: Null <: Name
  implicit val TermNameTag: ClassTag[TermName]

  /** The API of names that's supported on reflect mirror via an
   *  implicit conversion in reflect.ops
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
   */
  def newTermName(s: String): TermName

  /** Creates a new type name.
   */
  def newTypeName(s: String): TypeName

  /** Wraps the empty string. Can be used as the null object for term name.
   */
  def EmptyTermName: TermName = newTermName("")

  /** Wraps the empty string. Can be used as the null object for type name.
   */
  def EmptyTypeName: TypeName = EmptyTermName.toTypeName
}
