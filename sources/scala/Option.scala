package scala {

  trait Option[+a] {

    def get: a = this match {
      case None => error("None.get")
      case Some(x) => x
    }

    def map[b](f: a => b): Option[b] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }

    def flatMap[b](f: a => Option[b]): Option[b] = this match {
      case None => None
      case Some(x) => f(x)
    }

    def filter(p: a => boolean): Option[a] = this match {
      case None => None
      case Some(x) => if (p(x)) Some(x) else None
    }

    def foreach(f: a => Unit): Unit = this match {
      case None => ()
      case Some(x) => f(x)
    }

    def toList: List[a] = this match {
      case None => Predef.List()
      case Some(x) => Predef.List(x)
    }

    def print: Unit =
      System.out.println(this.toString())
  }
}
