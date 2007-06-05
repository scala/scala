trait Analyzer {
  val WILDCARD = "23"
}

trait Contexts2 { self: Analyzer =>
  class Context {
    def collect(sels: List[String]): List[String] = sels match {
      case List(WILDCARD) => val dummy = WILDCARD; Nil
    }
  }
}
