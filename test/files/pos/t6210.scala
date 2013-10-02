abstract sealed trait AST
abstract sealed trait AExpr                  extends AST
case class AAssign(name: String, v: AExpr)   extends AExpr
case class AConstBool(v: Boolean)            extends AExpr

trait Ty {}
case class TInt() extends Ty
case class TBool() extends Ty

object Foo {
  def checkExpr(ast: AExpr): Ty = {
    var astTy:Ty = ast match {
      case AAssign(nm: String, v:AExpr) => TBool()

      case AConstBool(v: Boolean) => TBool()

      case _                          => throw new Exception(s"Unhandled case check(ast: ${ast.getClass})")
    }
    astTy
  }
}
