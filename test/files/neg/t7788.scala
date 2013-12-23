trait T[A]

object O {
  implicit def conf[A]: T[A] = ???
}

class C {
  import O._
  def conf[A] = ""
  implicitly[T[String]] // should report shadowing under -Xlog-implicits
}
