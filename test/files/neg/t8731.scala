class C {
  // not a compile-time constant due to return type
  final val K: Int = 20

  def f(x: Int) = (x: @annotation.switch) match {
    case K => 0
    case 2 => 1
  }

  def g(x: Int) = (x: @annotation.switch) match {
    case K => 0
    case 2 => 1
    case 3 => 2
  }
}
