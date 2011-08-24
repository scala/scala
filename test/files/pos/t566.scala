object test {
  def foo[a](ys: List[a]): List[a] =
    return ys.head :: ys.tail
}
