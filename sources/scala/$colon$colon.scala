package scala {

  /* A non empty list.
  *
  */
  final case class ::[b](hd: b, tl: List[b]) extends List[b] with {
    def isEmpty = False;
    def head = hd;
    def tail = tl;
    override def toString(): String = mkString("[", ",", "]");
  }
}
