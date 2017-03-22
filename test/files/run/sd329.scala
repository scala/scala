object Test extends App {
  def d1: Double = 0.0
  def d2: Double = -0.0
  def d3: Double = Double.NaN
  def d4: Double = Double.NaN
  assert(d1 == d2)
  assert(d3 != d4)

  def d1B: java.lang.Double = d1
  def d2B: java.lang.Double = d2
  def d3B: java.lang.Double = d3
  def d4B: java.lang.Double = d4
  assert(d1B == d2B)
  assert(d1  == d1B)
  assert(d1B == d1)
  assert(d3B != d4B)
  assert(d3  != d4B)
  assert(d3B != d4)

  assert(!d1B.equals(d2B)) // ! see javadoc
  assert( d3B.equals(d4B)) // ! see javadoc

  def d1A: Any = d1
  def d2A: Any = d2
  def d3A: Any = d3
  def d4A: Any = d4
  assert(d1A == d2A)
  assert(d1  == d1A)
  assert(d1A == d1)
  assert(d1B == d1A)
  assert(d1A == d1B)

  assert(d3A != d4A)
  assert(d3  != d4A)
  assert(d3A != d4)
  assert(d3B != d4A)
  assert(d3A != d4B)


  def f1: Float = 0.0f
  def f2: Float = -0.0f
  def f3: Float = Float.NaN
  def f4: Float = Float.NaN
  assert(f1 == f2)
  assert(f3 != f4)

  def f1B: java.lang.Float = f1
  def f2B: java.lang.Float = f2
  def f3B: java.lang.Float = f3
  def f4B: java.lang.Float = f4
  assert(f1B == f2B)
  assert(f1  == f1B)
  assert(f1B == f1)
  assert(f3B != f4B)
  assert(f3  != f4B)
  assert(f3B != f4)

  assert(!f1B.equals(f2B)) // ! see javadoc
  assert( f3B.equals(f4B)) // ! see javadoc

  def f1A: Any = f1
  def f2A: Any = f2
  def f3A: Any = f3
  def f4A: Any = f4
  assert(f1A == f2A)
  assert(f1  == f1A)
  assert(f1A == f1)
  assert(f1B == f1A)
  assert(f1A == f1B)

  assert(f3A != f4A)
  assert(f3  != f4A)
  assert(f3A != f4)
  assert(f3B != f4A)
  assert(f3A != f4B)
}
