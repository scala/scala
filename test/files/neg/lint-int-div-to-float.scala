//> using options -Xlint -Xfatal-warnings

class C {
  def f = 1

  def w1: Double = f / 2
  def w2: Double = (f / 2) * 3
  def w3: Double = -(f / 2)
  def w4: Double = (new C).f / (new C).f * 3
  def w5: Double = f - f.abs / 2

  def o1: Double = (f / 2).toDouble
  def o2: Double = f.toDouble / 2
  def o3: Double = f / 2.toDouble
  def o4: Double = f / 2d
  def o5: Double = (new C).f.toDouble / (new C).f * 3
  def o6: Long = f / 2 + 3 // only warn if widening to a floating point, not when widening int to long
}
