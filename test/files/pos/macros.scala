object Test {

  class C {
    def macro foo[T](xs: List[T]): T = (T, xs) match {
      case (t1: Type, t2: Tree) => t2
    }
  }
}
