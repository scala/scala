object Test extends App {

  object UnapplySeqTest {
    def unapplySeq(any: Any): Option[(Int, Seq[Int])] = Some((5, Nil))
  }

  null match {
    case UnapplySeqTest(5) => println("Matched!")
  }
}
