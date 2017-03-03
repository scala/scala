trait Fun[A, B] { def apply(a: A): B }

class SamInferResult {
  def foreach[U](f: Fun[String, U]): U = ???
  def foo = foreach(println)
}