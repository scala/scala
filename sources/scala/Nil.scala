package scala {

  /* An empty list. Scala provides <code>[]</code> as syntactic sugar
  * for <code>Nil</code>.
  */
  case object Nil extends List[All] {
    def isEmpty = true;
    def head: All = error("head of empty list");
    def tail: List[All] = error("tail of empty list");
  }
}

