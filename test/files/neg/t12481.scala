
class V[+A]
object V {
  def empty[A] = new V[Nothing]
}

trait Txn[T <: Txn[T]]
trait Trivial extends Txn[Trivial]

trait Universe[T <: Txn[T]]

trait Ref[A]
object Ref {
  def apply[A](init: A)(implicit om: OptManifest[A]): Ref[A] = ???
}

trait Handler {
  def ok: Ref[V[Universe[Trivial]]] = Ref(V.empty)   // not here
  def nope: Ref[V[Universe[_]]]     = Ref(V.empty)   // here
}
