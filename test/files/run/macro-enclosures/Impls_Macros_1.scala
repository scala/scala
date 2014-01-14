import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
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