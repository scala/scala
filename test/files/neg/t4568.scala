object SubList {
  implicit def sublistable[A](x: List[A]) = new SubListable(x)

  class SubListable[A](x: List[A]) {
    def isSubListOf(y: List[A]) = {
      x match {
        case Nil => true
        case h :: t => y.contains(h) && (t.isSubListOf(y.drop(y.indexOf(h) + 1)))
      }
    }
  }

}