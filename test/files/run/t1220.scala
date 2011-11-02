object Test extends App {

  class QSRichIterable[A](self: Iterable[A]) {
    def filterMap[R](f: PartialFunction[A,R]) = 
      self filter (f.isDefinedAt) map f
  }

  object Un {
    def unapply(i: Int): Option[Int] = Some(i)
  }

  val richIter = new QSRichIterable(List(0, 1, 2, 3, 4))

  assert((richIter filterMap {case Un(3) => 7}) == List(7))
}
