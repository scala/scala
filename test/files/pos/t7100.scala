class Buffer {
  def f[@specialized(Int) T](): T = 0 match {
    case 0 => 0.asInstanceOf[T]
    case 1 => 0.asInstanceOf[T]
  }
}
