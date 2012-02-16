package scala.reflect
package api

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
  type Name >: Null <: AbsName

  /** The abstract type of names representing terms */
  type TypeName <: Name

  /** The abstract type of names representing types */
  type TermName <: Name

  abstract class AbsName {
    /** Is this name a term name? */
    def isTermName: Boolean

    /** Is this name a type name? */
    def isTypeName: Boolean

    /** Returns a term name that represents the same string as this name */
    def toTermName: TermName

    /** Returns a type name that represents the same string as this name */
    def toTypeName: TypeName

    /** Replaces all occurrences of $op_names in this name by corresponding operator symbols.
     *  Example: `foo_+=` becomes `foo_$plus$eq`.
     */
    def decoded: String

    /** Replaces all occurrences of operator symbols in this name by corresponding $op_names.
     *  Example: `foo_$plus$eq` becomes `foo_+=`
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

  def EmptyTermName: TermName = newTermName("")

  def EmptyTypeName: TypeName = EmptyTermName.toTypeName
}
