trait A {
  type Type
  type MethodType <: Type

  val MethodType: MethodTypeExtractor = null

  abstract class MethodTypeExtractor {
    def unapply(tpe: MethodType): Option[(Any, Any)]
  }
}

object Test {
  val a: A = null

  def foo(tpe: a.Type) = tpe match {
    case a.MethodType(_, _) =>
  }
}