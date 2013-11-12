import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext) = {
    import c.universe._
    reify {
      println("enclosingPackage = " + c.Expr[String](Literal(Constant(c.enclosingPackage.toString))).splice)
      println("enclosingClass = " + c.Expr[String](Literal(Constant(c.enclosingClass.toString))).splice)
      println("enclosingImpl = " + c.Expr[String](Literal(Constant(c.enclosingImpl.toString))).splice)
      println("enclosingTemplate = " + c.Expr[String](Literal(Constant(c.enclosingTemplate.toString))).splice)
      println("enclosingMethod = " + c.Expr[String](Literal(Constant(c.enclosingMethod.toString))).splice)
      println("enclosingDef = " + c.Expr[String](Literal(Constant(c.enclosingDef.toString))).splice)
    }
  }

  def foo = macro impl
}