package scala.reflect
package base

/** A trait that manages names.
 *  A name is a string in one of two name universes: terms and types.
 *  The same string can be a name in both universes.
 *  Two names are equal if they represent the same string and they are
 *  members of the same universe.
 *
 *  Names are interned. That is, for two names `name11 and `name2`,
 *  `name1 == name2` implies `name1 eq name2`.
 */
trait Names {

  /** The abstract type of names */
  type Name >: Null <: NameBase
  implicit val NameTag: ClassTag[Name]

  /** The abstract type of names representing terms */
  type TypeName >: Null <: Name
  implicit val TypeNameTag: ClassTag[TypeName]

  /** The abstract type of names representing types */
  type TermName >: Null <: Name
  implicit val TermNameTag: ClassTag[TermName]

  /** The base API that all names support */
  abstract class NameBase {
    /** Is this name a term name? */
    def isTermName: Boolean

    /** Is this name a type name? */
    def isTypeName: Boolean

    /** Returns a term name that represents the same string as this name */
    def toTermName: TermName

    /** Returns a type name that represents the same string as this name */
    def toTypeName: TypeName
  }

  /** Create a new term name.
   */
  def newTermName(s: String): TermName

  /** Creates a new type name.
   */
  def newTypeName(s: String): TypeName

  def EmptyTermName: TermName = newTermName("")

  def EmptyTypeName: TypeName = EmptyTermName.toTypeName
}
