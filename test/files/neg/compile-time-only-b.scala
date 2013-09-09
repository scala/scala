import scala.reflect.runtime.universe._

object Test extends App {
  // HAHA!!!
  // no compileTimeOnly errors here, because scalac does constant folding
  // the type of reify(42) is Expr[42.type]
  // therefore the type of expr.splice is 42.type, which is then constfolded
  val expr = reify(42)
  val ignored1 = expr.splice
  val ignored2 = expr.value

  val fortyTwo = 42
  val ignored3 = reify(fortyTwo).splice
  val ignored4 = reify(fortyTwo).value
}