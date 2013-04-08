class C1 {
  type T
  def this(x: T) { this() }
}

class C1a[T] {
  def this(x: T) { this() } // works, no error here
}

class C2(x: Int) {
   def this(a: Int, b: Int = x) {
     this(b)
  }
}

class C3 {
  val x = 0
  def this(a: Int = x) { this() }
}
