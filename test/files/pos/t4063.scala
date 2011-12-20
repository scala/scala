trait Parallel
trait Parallelizable[+ParRepr <: Parallel]

trait PIterableLike[+T, +Repr <: Parallel] extends Parallel with Parallelizable[PIterableLike[T, Repr]]

trait PMap[K, V] extends PIterableLike[(K, V), PMap[K, V]]
trait PSet[T] extends PIterableLike[T, PSet[T]]

trait CIterableLike[+T, +Repr]

trait CSet[T] extends CIterableLike[T, CSet[T]] with Parallelizable[PSet[T]]

trait CMap[K, V] extends CIterableLike[(K, V), CMap[K, V]] with Parallelizable[PMap[K, V]]

object Test {
  var x = 0

  def main() {
    val map: CMap[Int, CSet[Int]] = new CMap[Int, CSet[Int]] {}
    val set: CSet[Int] = new CSet[Int] {}

    // should infer type argument
    //map.synchronized[CIterableLike[Any, Any] with Parallelizable[PIterableLike[Any, Parallel with Parallelizable[Parallel]]]] {
    // or:
    //map.synchronized[CIterableLike[Any, Any] with Parallelizable[PIterableLike[Any, Parallel]]] {
    // or, maybe it could also infer existential types:
    //map.synchronized[CIterableLike[Any, _] with Parallelizable[PIterableLike[Any, _]]] {

    map.synchronized {
      if (x == 0) {
        map
      } else {
        set
      }
    }

  }
}

