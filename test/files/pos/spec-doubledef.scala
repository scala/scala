trait A[@specialized(Int) T] {
  var value: T
  def getWith[@specialized(Int) Z](f: T => Z) = f(value)
}

class C extends A[Int] {
  var value = 10
  override def getWith[@specialized(Int) Z](f: Int => Z) = f(value)
}
