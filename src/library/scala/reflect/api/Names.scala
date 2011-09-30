package scala.reflect
package api

trait Names {

  type Name >: Null <: AbsName
  type TypeName <: Name
  type TermName <: Name

  abstract class AbsName {
    def isTermName: Boolean
    def isTypeName: Boolean
    def toTermName: TermName
    def toTypeName: TypeName

    /** Replace all occurrences of $op_names in this name by corresponding operator symbols.
     *  Example: `foo_+=` becomes `foo_$plus$eq`.
     */
    def decode: String

    /** Replace all occurrences of operator symbols in this name by corresponding $op_names.
     *  Example: `foo_$plus$eq` becomes `foo_+=`
     */
    def encode: Name
  }

  def newTermName(s: String): TermName
  def newTypeName(s: String): TypeName

  def EmptyTermName: TermName = newTermName("")
  def EmptyTypeName: TypeName = EmptyTermName.toTypeName
}
