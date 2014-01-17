class A {
  def fn1 = List apply 1
  def fn2 = List apply[Int] 2

  def g1: Char = "g1" toList 0
  def g2: Char = "g2" apply 1

  def h1 = List apply[List[Int]] (List(1), List(2)) mapConserve[List[Any]] (x => x)
}
