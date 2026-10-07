//> using options -Wunused:patvars -Werror

case class A(x: Int, y: Int)

object Main {
  for {
    a <- List.empty[A]
    A(x, y) = a
  } yield x + y

  private val A(x, y) = A(42, 27) // nowarn for canonical name
  private val A(w, z) = A(42, 27) // warn
  private[this] val A(q, r) = A(42, 27) // warn
  def W = w
  def Q = q
}

class C {
  def f(x: Any) =
    x match {
      case x: String => // nowarn because x is not a new reference but an alias
      case _ =>
    }
  def g(x: Any) =
    (x: @unchecked) match {
      case x: String => // nowarn because x is not a new reference but an alias
      case _ =>
    }
  def s(x: Option[String]) =
    x match {
      case x: Some[String] => // nowarn because x is not a new reference but an alias
      case _ =>
    }
  def t(x: Option[String]) =
    x match {
      case Some(x) => // nowarn because x is not a new reference but an alias of sorts
      case _ =>
    }
  val email = "(.*)@(.*)".r
  def spam(s: String) =
    s match {
      case email(s, addr) => // warn // warn each, multiple extraction
      case _ =>
    }
  def border(s: String) =
    s match {
      case email(s, _) => // nowarn only one patvar
      case _ =>
    }
  def `scala-dev#902`(v: (Int, (Boolean, String))): Unit =
    v match {
      case (i, v @ (_, _)) => i // warn multiple patvars
    }
}

final class ArrayOps[A](private val xs: Array[A]) extends AnyVal {
  def f =
    (xs: Array[_]) match {
      case xs =>
    }
}

class Publix {
  val A(w, z) = A(42, 27) // nowarn if an accessor is neither private nor local
}
