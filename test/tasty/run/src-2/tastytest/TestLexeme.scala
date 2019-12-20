package tastytest

object TestLexeme extends Suite("TestLexeme") {

  test(assert(Lexeme.empty < Lexeme("abc").get))
  test(assert(Lexeme("aaa").get < Lexeme("ab").get))
  test(assert((Lexeme("aaa").get compare Lexeme("ab").get) < 0))

}
