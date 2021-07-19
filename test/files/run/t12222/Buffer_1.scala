trait ABuffer[@specialized(Float)T] {
  def count: Int
}

class Buffer[@specialized(Float) T](array_par: Array[T]) extends ABuffer[T] {
  var array: Array[T] = array_par
  var count: Int = 0
}

class Float32Buffer(array_par: Array[Float]) extends Buffer[Float](array_par)