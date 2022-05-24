// scalac: -Werror -Wunused

class Child extends Parent_1 {
  def takeMany(values: String*): Unit = println()
  def takePoly[A](values: A*): Unit = println()
  def takeOne(value: String): Unit = println()
}
