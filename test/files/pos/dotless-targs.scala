class A {
  def fn1 = List apply 1
  def fn2 = List apply[Int] 2

  def f1 = "f1" isInstanceOf[String]

  def g1 = "g1" toList
  def g2 = "g2" toList 2
  def g3 = "g3" apply 3

  def h1 = List apply[List[Int]] (List(1), List(2)) mapConserve[List[Any]] (x => x)
}
