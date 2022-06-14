// scalac: -Werror -Wunused

class Child extends Parent_1 {
  def takeMany(values: String*): Unit = println()   // error in RefChecks without linting
}
