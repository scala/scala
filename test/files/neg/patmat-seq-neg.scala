object A {
  def unapplySeq(a: Int) = Some(collection.mutable.ArrayBuffer(1,2,3))
}
object B {
  def unapplySeq(a: Int) = Some(Array(1,2,3))
}

class T {
  def t1: Any = 2 match {
    case A(xs@_*) => xs // ok
  }
  def t2: Any = 2 match {
    case A(x, y) => (x, y) // ok
  }
  def t3: Any = 2 match {
    case A(x, xs@_*) => (x, xs) // type error with call to drop. found: ArrayBuffer, required: Seq.
  }
  def t4: Any = 2 match {
    case B(xs@_*) => xs // error: toSeq is not a member of Array. no ArrayOps because adaptToMember is disabled after typer.
  }
  def t5: Any = 2 match {
    case B(x, y) => (x, y) // ok
  }
  def t6: Any = 2 match {
    case B(x, xs@_*) => (x, xs) // error: drop is not a member of Array
  }
}
