object test {
  type z[a,b] = a => b
  def f : z[int,int] = (i => i + 1)
}