object test {
  type z[a, b] = a => b
  def f : z[Int, Int] = (i => i + 1)
}
