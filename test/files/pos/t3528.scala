class A {
  // 3528 - not fixed
  // def f1 = List(List(1), Stream(1))
  // 3528 comments
  def f2 = List(Set(1,2,3), List(1,2,3))
  // 2322
  def f3 = List(null: Range, null: List[Int])  
}
