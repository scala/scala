object seqapply_convention { // 2005-02-17 see comment in PatternMatcher::isSeqApply

  val x:Seq[Int] = List(1,2,3);

  x.match {
    case List(1,2,3) => true // type error: type Seq will not work
  }
}
