package scala {

  /* An empty list. Scala provides <code>[]</code> as syntactic sugar
  * for <code>Nil</code>.
  */
  final case class Nil[c]() extends List[c] {
    def isEmpty = true;
    def head: c = error("head of empty list");
    def tail: List[c] = error("tail of empty list");
    override def toString(): String = "[]";
  }
}

