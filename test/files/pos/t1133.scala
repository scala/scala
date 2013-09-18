object Match
{
  def main(args: Array[String]) = {
    args(0) match {
        case Extractor1(Extractor2(Extractor3("dog", "dog", "dog"), x2, x3), b, c, Extractor3("b", "b", f), e) => println(e)
        case Extractor3(Extractor2(Extractor1("a", "aa", "aaa", "aa", "a"), Extractor2("a", "aa", "aaa"), e), y, z) => println(e)
        case Extractor2(Extractor3("a", "a", x), Extractor3("b", "b", y), Extractor3("c", "c", z)) => println(z)
        case _ => println("fail")
    }
  }

  object Extractor1 {
    def unapply(x: Any) = x match {
        case x: String => Some(x, x+x, x+x+x, x+x, x)
        case _ => None
    }
  }

  object Extractor2 {
    def unapply(x: Any) = x match {
        case x: String => Some(x, x+x, x+x+x)
        case _ => None
    }
  }

  object Extractor3 {
    def unapply(x: Any) = x match {
        case x: String => Some(x, x, x)
        case _ => None
    }
  }
}
