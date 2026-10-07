
class Parent {
  private val field: Int = 3
}

class Child(n: Int) extends {
  private val field = n
} with Parent {
  class Inner {
    def f = field
  }
}
