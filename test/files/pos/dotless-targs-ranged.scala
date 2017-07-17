class A {
  def fn1 = List apply 1
  def fn2 = List apply[Int] 2

  def g1: Char = "g1" toList 0
  def g2: Char = "g2" apply 1

  def h1 = List apply[List[Int]] (List(1), List(2)) mapConserve[List[Any]] (x => x)

  def op[A, B](i: Int): Int = 2*i

  def eval = 1 ->[Int] 2
  def evil = new A() op  [Int,   String     ]  42
}

