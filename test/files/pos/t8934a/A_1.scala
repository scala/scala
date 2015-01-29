import language.experimental.macros
import reflect.macros.whitebox.Context

object Unapply {
  def impl1(c: Context)(a: c.Tree): c.Tree = {
    import c.universe._
    q"(new { def unapply[T](a: String): Option[(Int, String)] = ??? }).unapply($a)"
  }
  def unapply(a: Any): Any = macro impl1
}

object UnapplySeq {
  def impl1(c: Context)(a: c.Tree): c.Tree = {
    import c.universe._
    q"(new { def unapplySeq[T](a: String): Option[(Int, Seq[String])] = ??? }).unapplySeq($a)"
  }
  def unapplySeq(a: Any): Any = macro impl1
}
