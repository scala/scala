package tastytest

object TestLexeme {

  def test1 = assert(Lexeme.empty < Lexeme("abc").get)
  def test2 = assert(Lexeme("aaa").get < Lexeme("ab").get)

  def main(args: Array[String]): Unit = {
    test1
    test2
    println("Suite passed!")
  }
}
