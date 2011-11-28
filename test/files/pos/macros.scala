object Test {

  class C { 
    def macro foo[T](xs: List[T]): T = (T, xs) match {
      case (t1: glob.Type, t2: glob.Tree) => t2
    }
  }
}


