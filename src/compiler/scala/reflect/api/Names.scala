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

  def newTermName(cs: Array[Char], offset: Int, len: Int): TermName
  def newTermName(cs: Array[Byte], offset: Int, len: Int): TermName
  def newTermName(s: String): TermName

  def newTypeName(cs: Array[Char], offset: Int, len: Int): TypeName
  def newTypeName(cs: Array[Byte], offset: Int, len: Int): TypeName
  def newTypeName(s: String): TypeName

  implicit def promoteTermNamesAsNecessary(name: Name): TermName = name.toTermName

  def EmptyTermName: TermName = newTermName("")
  def EmptyTypeName: TypeName = EmptyTermName.toTypeName
}

