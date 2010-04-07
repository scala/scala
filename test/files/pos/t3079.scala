sealed trait Identity[A] {
  val value: A
}

trait Coerce[A, B] {
  def unwrap: (A => B)
}

object Coerce {
  def IdentityCoerce[B] = new Coerce[Identity[B], B] {
     // java.lang.Error: A in trait Identity cannot be instantiated from ?x$1.type
     def unwrap = _.value

     // Providing the type of _ works around the problem.
     //def unwrap = (_: Identity[B]).value
  }
}