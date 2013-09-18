object Test  {

  def testCons: Unit = {
    def x[A](a: PartialFunction[Any, A]): A = a(0)
    val eval0 = x { case list: List[Int @unchecked] => list }
  }
}
