import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Macros {
  def id[A]: A = null.asInstanceOf[A]

  def foo: Any = macro impl
  def impl(c: Context): c.Tree = {
    import c.universe._
    import Flag._

    lazy val tpe = TypeTree(typeOf[Int])

    /* If we used this line instead, it would work! */
    // lazy val tpe = tq"Int"

    lazy val param: ValDef = {
      val p1 = q"val a: ${tpe.duplicate} = Macros.id[${tpe.duplicate}]"
      ValDef(Modifiers(DEFAULTPARAM), p1.name, p1.tpt, p1.rhs)
    }

    q"""
      class C { def f($param) = a }
      println(new C().f())
    """
  }
}
