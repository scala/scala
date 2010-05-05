
// ticket #3378, overloaded specialized variants
import scala.collection.{Traversable,TraversableLike};
import scala.collection.generic.CanBuildFrom;

trait PartialMap[@specialized A,@specialized B]
extends PartialFunction[A,B] with Iterable[(A,B)] {

   // commenting out this declaration gives a different exception.
  /** Getter for all values for which the given key function returns true. */
   def apply(f : (A => Boolean)) : Iterator[B] =
     for ((k,v) <- iterator; if f(k)) yield v;

  // if this is commented, it compiles fine:
  def apply[This <: Traversable[A], That](keys : TraversableLike[A,This])
  (implicit bf: CanBuildFrom[This, B, That]) : That = keys.map(apply);
}
