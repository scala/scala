trait RuleLang {
  object __match {
    def one[T](x: T): List[T] = List(x)
    def zero = Nil
    def guard[T](cond: Boolean, result: => T): List[T] =
      if (cond) List(result) else Nil
    def runOrElse[T, U](in: T)(matcher: T => List[U]): List[U] =
      matcher(in)
  }

  def infix_orElse[A](lst: List[A], alternative: => List[A]): List[A] =
    lst ++ alternative

  class Rule[A,B](f: A => List[B]) {
    def unapply(x: A): List[B] = f(x)
  }

  def infix_rule[A, B](f: A => List[B]): Rule[A,B] = new Rule(f)

  lazy val && = ((x: String) => x match {
    case x => (x,x)
  }) rule

  lazy val Likes = ((x: String) => x match {
    case "A" => "Coffee"
    case "B" => "Coffee"
    case "D" => "Coffee"
    case "E" => "Coffee"
  }) rule

  lazy val Friend = ((x: String) => x match {
    case "A" => "C"
    case "C" => "D"
    case "B" => "D"
    case "A" => "E"
  }) rule

  lazy val Knows: Rule[String, String] = ((x: String) => x match {
    case Friend(Knows(y)) => y
    case x => x
  }) rule

  lazy val ShouldGrabCoffee = ((x: String) => x match {
    case Likes("Coffee") && Knows(y @ Likes("Coffee")) if x != y =>
      x + " and " + y + " should grab coffee"
  }) rule
}

object Test extends App with RuleLang {
  println("ONE")
  ("A" match {
    case ShouldGrabCoffee(r) => r
  }).foreach(println)

  println("TWO")
  val ShouldGrabCoffee(r) = "A"
  r.foreach(println)

  println("THREE")
  List("A", "B", "C", "D").flatMap({
    case ShouldGrabCoffee(r) => r
  }).foreach(println)
}
