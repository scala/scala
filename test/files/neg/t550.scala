abstract class Monoid[a] {
  def unit: a
}

object test {
  def sum[a](xs: List)(implicit m: Monoid[a]): a =
    if (xs.isEmpty) m.unit else xs.head
  sum(List(1,2,3))
}
