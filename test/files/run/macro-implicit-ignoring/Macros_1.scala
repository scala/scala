import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait TypeClass[A] {

  def value: A
}

object TypeClass {
  
  implicit val defaultString: TypeClass[String] = new TypeClass[String] {
    def value: String = "implicit-in-companion"
  }

  def ignoreDefault[A]: A = macro ignoreDefaultImpl[A]

  def ignoreDefaultImpl[A](c: blackbox.Context)(implicit tt: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._

    val defaultStringSymbol = c.weakTypeOf[TypeClass.type].decl(TermName("defaultString"))

    scala.util
      .Try(c.inferImplicitValueIgnoring(weakTypeOf[TypeClass[A]], silent = true, withMacrosDisabled = false)(defaultStringSymbol))
      .toOption
      .filterNot(_ == EmptyTree) match {
        case Some(default) => c.Expr[A](q"$default.value")
        case None if weakTypeOf[A] <:< weakTypeOf[String] => c.Expr[A](q""" "ignoring-implicit-in-companion" """)
        case None => c.abort(c.enclosingPosition, "No implicit value found")
      }
  }
}
