import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class infer extends scala.annotation.StaticAnnotation
object infer {
  def value[T]: T = macro Macro.inferValue
}
object Macro {
  // Like `infer.value` in scala-commons: check that the default is that of an `@infer` parameter by looking at all
  // annotations of the parameter as trees.
  def inferValue(c: blackbox.Context): c.Tree = {
    import c.universe._
    val owner = c.internal.enclosingOwner
    val Default = """.*\$default\$(\d+)""".r
    val param = owner.name.decodedName.toString match {
      case Default(i) =>
        val ctor = owner.owner.companion.info.decl(termNames.CONSTRUCTOR)
        ctor.asMethod.paramLists.flatten.apply(i.toInt - 1)
      case _ => owner
    }
    if (param.annotations.count(_.tree.tpe <:< typeOf[infer]) != 1)
      c.abort(c.enclosingPosition, s"not an @infer parameter: $param")
    q"""throw new NotImplementedError("infer.value")"""
  }
}
