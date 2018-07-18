
case class User(name: String, age: Int)

object Test extends App {
  def pretty(p: Product): String =
    p.productElementNames.zip(p.productIterator)
     .map { case (name, value) => s"$name=$value" }
     .mkString(p.productPrefix + "(", ", ", ")")
  println(pretty(User("Susan", 42)))
}

