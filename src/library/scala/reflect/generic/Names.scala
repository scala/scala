package scala.reflect
package generic

trait Names {
  type Name >: Null <: AnyRef
  type TypeName <: Name
  type TermName <: Name

  def newTermName(cs: Array[Char], offset: Int, len: Int): TermName
  def newTermName(cs: Array[Byte], offset: Int, len: Int): TermName
  def newTermName(s: String): TermName
  def mkTermName(name: Name): TermName

  def newTypeName(cs: Array[Char], offset: Int, len: Int): TypeName
  def newTypeName(cs: Array[Byte], offset: Int, len: Int): TypeName
  def newTypeName(s: String): TypeName
  def mkTypeName(name: Name): TypeName

  def isTermName(name: Name): Boolean
  def isTypeName(name: Name): Boolean

  implicit def promoteTermNamesAsNecessary(name: Name): TermName = mkTermName(name)
}

