package tastytest

import SymbollicEnums._

object TestSymbollicEnums extends Suite("TestSymbollicEnums") {

  implicit object Interpreter extends Interpreter[Long] {

    val zero = 0L.lit
    val one  = 1L.lit

    def interpret(e: Expr[Long]): Long = e match {
      case Expr.Literal(x)      => x
      case Expr.Infix(op, l, r) => op match {
        case Ops.+ => interpret(l) + interpret(r)
        case Ops.* => interpret(l) * interpret(r)
      }
    }
  }

  val Laws = new Laws[Long]
  import Laws._

  test(assert(additiveIdentity(getRandomNat.toLong.lit)))
  test(assert(additiveCommutativity(getRandomNat.toLong.lit, getRandomNat.toLong.lit)))
  test(assert(additiveAssociativity(getRandomNat.toLong.lit, getRandomNat.toLong.lit, getRandomNat.toLong.lit)))
  test(assert(multiplicativeIdentity(getRandomNat.toLong.lit)))
  test(assert(multiplicativeCommutativity(getRandomNat.toLong.lit, getRandomNat.toLong.lit)))
  test(assert(multiplicativeAssociativity(getRandomNat.toLong.lit, getRandomNat.toLong.lit, getRandomNat.toLong.lit)))
  test(assert(multiplicativeDistributivity(getRandomNat.toLong.lit, getRandomNat.toLong.lit, getRandomNat.toLong.lit)))


  override val reps = 100_000

}
