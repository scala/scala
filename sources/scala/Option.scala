package scala {

  trait Option[a] {

    def isNone: Boolean;
    def get: a;

    def map[b](f: a => b): Option[b] = this match {
      case None() => None()
      case Some(x) => Some(f(x))
    }

    def toList: List[a] = this match {
      case None() => Predef.List()
      case Some(x) => Predef.List(x)
    }

    def print: Unit =
      System.out.println(this.toString())
  }
}