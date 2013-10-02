class A
object A {
  def unapply(a: A): Some[A] = Some(a) // Change return type to Option[A] and the warning is gone
}

object Test {
  for (A(a) <- List(new A)) yield a // spurious dead code warning.
}

// List(new A()).withFilter(((check$ifrefutable$2) => check$ifrefutable$2: @scala.unchecked match {
//  case A((a @ _)) => true
//  case _ => false // this is dead code, but it's compiler generated.
// }))
