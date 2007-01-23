abstract class A[T <% Ordered[T]] {
  def makeA = new Object
//  case object default extends Object
}

class C[T <% Ordered[T]](foo: Object) extends A[T] {
  def this() = {
    this(makeA)
    // this(default)
  }
}
