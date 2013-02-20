import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context)(target: c.Tree, name: c.Tree, code: c.Tree) = {
    import c.universe._
    val Literal(Constant(targetType: Type)) = c.typeCheck(target)
    val Literal(Constant(methodName: String)) = name
    val Function(methodParams, methodBody) = code
    val method = DefDef(NoMods, TermName(methodName), Nil, List(methodParams), TypeTree(), methodBody)
    c.introduceMember(targetType.typeSymbol, method)
    c.literalUnit
  }

  def addMethod(target: _, name: String, code: _)  = macro impl
}