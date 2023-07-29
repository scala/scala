
// scalac: -Werror -Xlint:infer-any

class C {
  val b = collection.mutable.ListBuffer.empty[Int]
  def f(i: Int, t: Boolean) = if (t) b += i else 42 // warn because if in expr position
  def g[A](a: A): A = a
  def h(i: Int, t: Boolean) = g(if (t) k) // warn because expr
  def k: Int = 27
  def n(i: Int, t: Boolean) =
    t match { // warn expr
      case true => b += i
      case _ => 42
    }
  def z = List(42, "hello") // warn type inference as usual

  def `arms are any`(xs: List[String]) =
    (xs: Any) match { // nowarn because only top/bottom types
      case null => throw new NullPointerException // null-check first helps static analysis of instanceOf
      case y: Any => y
    }
  def `arms are any but`(xs: List[String]) =
    xs match { // warn because interesting type of arm
      case null => throw new NullPointerException // null-check first helps static analysis of instanceOf
      case _ :: Nil => new Object
      case List(_ @ _*) => ""
    }
}
