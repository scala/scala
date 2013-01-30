object Test extends scala.util.parsing.combinator.JavaTokenParsers {

    def test[A](s: String) {
      val res = parseAll(ident, s) match {
        case Failure(_, in) => Failure("java identifier expected", in)
        case o => o
      }
      println(res)
    }

    def main(args: Array[String]) {
      // Happy tests
      test("simple")
      test("with123")
      test("with$")
      test("withøßöèæ")
      test("with_")
      test("_with")
      // Sad tests
      test("3start")
      test("-start")
      test("with-s")
      test("we♥scala")
      test("with space")
    }
}
