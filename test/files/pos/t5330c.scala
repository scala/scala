object t5330c {
  val s: Set[_ >: Char] = Set('A')
  s forall ("ABC" contains _)
  s.forall( c => "ABC".toSeq.contains( c ))
}
