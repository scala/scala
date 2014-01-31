import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => ru}

object StaticReflect {
  def method[A](name: String): ru.Type = macro methodImpl[A]

  def methodImpl[A: c.WeakTypeTag](c: Context)(name: c.Expr[String]): c.Expr[ru.Type] = {
    import c.universe._
    import internal._

    val nameName: TermName = name.tree match {
      case Literal(Constant(str: String)) => TermName(str)
      case _                              => c.error(c.enclosingPosition, s"Method name not constant.") ; return reify(ru.NoType)
    }
    val clazz  = weakTypeOf[A]

    clazz member nameName match {
      case NoSymbol => c.error(c.enclosingPosition, s"No member called $nameName in $clazz.") ; reify(ru.NoType)
      case member   =>
        val mtpe  = member infoIn clazz
        val mtag  = c.reifyType(gen.mkRuntimeUniverseRef, Select(gen.mkRuntimeUniverseRef, TermName("rootMirror")), mtpe)
        val mtree = Select(mtag, TermName("tpe"))

        c.Expr[ru.Type](mtree)
    }
  }

}
