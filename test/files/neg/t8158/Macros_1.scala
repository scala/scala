import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Max {
  def impl(c: Context)(any: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._
    def fail(msg: String) = c.abort(c.enclosingPosition, msg)
    val t = c.macroApplication match {
      case q"$_.unapply($unargs)" =>
        /* hangs
        */
        q"""
          new {
            def isEmpty = false
            def get = this
            def unapply(x: String) = this
          }.unapply($unargs)
        """
        /*
        if get returns Unit or Boolean:
        wrong number of patterns for <$anon: AnyRef> offering Unit: expected 1, found 0
        */
        /* straightforward
        q"""
          new {
            def unapply(x: String) = true
          }.unapply($unargs)
        """
        */
      case _ => fail("bad appl")
    }
    c.Expr[Any](t)
  }
}