package scala with {

  abstract class Option[a] with {

    abstract def isNone: Boolean;
    abstract def get: a;

    def map[b](f: (a)b): Option[b] = this match {
      case None => this.as[Option[b]]
      case Some(x) => Some(f(x))
    }

    def toList: List[a] = this match {
      case None => []
      case Some(x) => [x]
    }

    def print: Unit =
      System.out.println(this.toString())
  }
}