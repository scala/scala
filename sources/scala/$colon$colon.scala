package scala {

  /* A non empty list.
  *
  */
  final case class ::[b](hd: b, tl: List[b]) extends List[b] {
    def isEmpty: boolean = false;
    def head: b = hd;
    def tail: List[b] = tl;
  }
}
