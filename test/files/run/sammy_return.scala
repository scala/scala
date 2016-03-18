trait Fun[A, B] { def apply(a: A): B }
class PF[A, B] { def runWith[U](action: Fun[B, U]): Fun[A, Boolean] = a => {action(a.asInstanceOf[B]); true} }

class TO[A](x: A) {
  def foreach[U](f: Fun[A, U]): U = f(x)
  def collectFirst[B](pf: PF[A, B]): Option[B] = {
    foreach(pf.runWith(b => return Some(b)))
    None
  }
}

object Test extends App {
  assert(new TO("a").collectFirst(new PF[String, String]).get == "a")
}