package tastytest

object TestOperatorToken extends Suite("TestOperatorToken") {
  test(assert(OperatorToken.<:< != null))
  test(assert(OperatorToken.=:= != null))
  test(assert(OperatorToken.<*> != null))
}
