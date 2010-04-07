package scala.reflect
package generic

trait Names {

  type Name >: Null <: AnyRef

  def newTermName(cs: Array[Char], offset: Int, len: Int): Name
  def newTermName(cs: Array[Byte], offset: Int, len: Int): Name
  def newTermName(s: String): Name

  def mkTermName(name: Name): Name

  def newTypeName(cs: Array[Char], offset: Int, len: Int): Name
  def newTypeName(cs: Array[Byte], offset: Int, len: Int): Name
  def newTypeName(s: String): Name

  def mkTypeName(name: Name): Name
}


