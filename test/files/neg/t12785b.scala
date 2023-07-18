
class C {
  def f: Nothing = null.asInstanceOf[Predef.type]
  def g: Nothing = null.asInstanceOf[scala.`package`.type]
  def list: Nothing = null.asInstanceOf[List.type]
  def set: Nothing = null.asInstanceOf[Set.type]
  def nil: Nothing = null.asInstanceOf[Nil.type]
}
