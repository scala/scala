class Test {
  trait Q[T] {
    def toArray[T](x: Array[T]): Array[T]
    def toArray(): Array[T]
  }

  def crashTyper: Array[_] = {
   val x : Q[_] = ???
   x.toArray // crashes while doing overload resolution
  }
}