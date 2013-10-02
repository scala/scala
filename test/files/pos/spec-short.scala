abstract class AbsFun[@specialized T, @specialized U] {
  // abstract function, fully specialized
  def apply(x: T): U

  // abstract function, specialized
  def sum(xs: List[T]): Int

  def prod(xs: List[T], mul: (Int, T) => Int): Int =
    (1 /: xs)(mul)

  // concrete function, not specialized
  def bar(m: String): String = m

  // abstract function, not specialized
  def abs(m: Int): Int
}

class Square extends AbsFun[Int, Int] {
  def apply(x: Int): Int = x * x

  def sum(xs: List[Int]): Int =
    (0 /: xs) (_ + _)

  def abs(m: Int): Int =
    sum(List(1, 2, 3))
}
