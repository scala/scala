package scala {

  /* A non empty list.
  *
  */
  final case class ::[b, c <: b](hd: b, tl: List[c]) extends List[b] {
    def isEmpty: boolean = false;
    def head: b = hd;
    def tail: List[b] = tl;
  }
}
