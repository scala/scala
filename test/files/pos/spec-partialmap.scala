// ticket #3378, overloaded specialized variants
import scala.collection.{Iterable,IterableOps};

trait PartialMap[@specialized A,@specialized B] extends PartialFunction[A,B] with Iterable[(A,B)] {
   // commenting out this declaration gives a different exception.
  /** Getter for all values for which the given key function returns true. */
   def apply(f : (A => Boolean)) : Iterator[B] =
     for ((k,v) <- iterator; if f(k)) yield v

  // if this is commented, it compiles fine:
  def apply[This <: Iterable[A]](keys : IterableOps[A, Iterable, This]): Iterable[B] = keys.map(apply)
}
