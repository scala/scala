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
  }

  def newTermName(s: String): TermName
  def newTypeName(s: String): TypeName

  def EmptyTermName: TermName = newTermName("")
  def EmptyTypeName: TypeName = EmptyTermName.toTypeName
}
