import scala.reflect.macros.Context

object Macros {
  // params

  def implParamUntypedExpr(c: Context)(x: c.Expr[Int]) = x
  def fooParamUntypedExpr(x: _) = macro implParamUntypedExpr

  def implParamUntypedTree(c: Context)(x: c.Tree) = x
  def fooParamUntypedTree(x: _) = macro implParamUntypedTree

  def implParamTypedExpr(c: Context)(x: c.Expr[Int]) = x
  def fooParamTypedExpr(x: Int) = macro implParamTypedExpr

  def implParamTypedTree(c: Context)(x: c.Tree) = x
  def fooParamTypedTree(x: Int) = macro implParamTypedTree

  // return types

  def implRetUntypedExpr(c: Context): c.Expr[Int] = c.literal(42)
  def fooRetUntypedExpr: _ = macro implRetUntypedExpr

  def implRetUntypedTree(c: Context): c.Tree = c.literal(42).tree
  def fooRetUntypedTree: _ = macro implRetUntypedTree

  def implRetTypedExpr(c: Context): c.Expr[Int] = c.literal(42)
  def fooRetTypedExpr: Int = macro implRetTypedExpr

  def implRetTypedTree(c: Context): c.Tree = c.literal(42).tree
  def fooRetTypedTree: Int = macro implRetTypedTree
}