//> using options -Werror -Xlint:deprecation

// warn about introducing Unit-valued fields instead of merely side-effecting.
// warn about implicit not introducing any implicits
//
class C {
  val _ = 42
  val Some(_) = Option(42)
  implicit val _ = 42
  implicit val Some(_) = Option(42)

  val p = println()  // nowarn
  val Some(answer @ _) = Option(42)  // nowarn

  def f(i: 42) = {
    val _ = i                  // nowarn
    val Some(_) = Option(i)    // nowarn
    implicit val _ = i
    implicit val Some(_) = Option(i)
  }
}
object C {
  val Some(_) = Option(42)
  implicit val Some(_) = Option(42)
}
