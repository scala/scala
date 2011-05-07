class NoPredef {
  def f1 = 5L: java.lang.Long
  def f2 = new java.lang.Long(5) : Long
  def f3 = "abc" map (_ + 1)
}