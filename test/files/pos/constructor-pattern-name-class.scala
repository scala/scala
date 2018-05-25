case class ClassDef(a: Any)

trait T {
  def ClassDef(a: Any): Any
}
class C extends T {
  def ClassDef(a: Any) = a match {
    case t @ ClassDef(_) => t // when typing constructor pattern, we skip method symbols
  }
}
