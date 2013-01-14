import Macros._

object Test extends App {
  println(fooParamUntypedExpr(42))
  println(fooParamUntypedTree(42))
  println(fooParamTypedExpr(42))
  println(fooParamTypedTree(42))

  println(fooRetUntypedExpr)
  println(fooRetUntypedTree)
  println(fooRetTypedExpr)
  println(fooRetTypedTree)
}