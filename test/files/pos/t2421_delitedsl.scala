trait DeliteDSL {
  abstract class <~<[-From, +To] extends (From => To)
  implicit def trivial[A]: A <~< A = new (A <~< A) {def apply(x: A) = x}

  trait Forcible[T]
  object Forcible {
    def factory[T](f: T => Forcible[T]) = new (T <~< Forcible[T]){def apply(x: T) = f(x)}
  }

  case class DeliteInt(x: Int) extends Forcible[Int]
  implicit val forcibleInt = Forcible.factory(DeliteInt(_: Int))

  import scala.collection.Traversable
  class DeliteCollection[T](val xs: Traversable[T]) {
    // must use existential in bound of P, instead of T itself, because we cannot both have:
        // Test.x below: DeliteCollection[T=Int] -> P=DeliteInt <: Forcible[T=Int], as T=Int <~< P=DeliteInt
        // Test.xAlready below: DeliteCollection[T=DeliteInt] -> P=DeliteInt <: Forcible[T=DeliteInt], as T=DeliteInt <~< P=DeliteInt
        // this would required DeliteInt <: Forcible[Int] with Forcible[DeliteInt]

    def headProxy[P <: Forcible[_]](implicit w: T <~< P): P = xs.head
  }
  // If T is already a proxy (it is forcible), the compiler should use
  // forcibleIdentity to deduce that P=T.  If T is Int, the compiler
  // should use intToForcible to deduce that P=DeliteInt.
  //
  // Without this feature, the user must write 'xs.proxyOfFirst[DeliteInt]',
  // with the feature they can write 'xs.proxyOfFirst', which is shorter and
  // avoids exposing internal DELITE types to the world.

  object Test {
    val x = new DeliteCollection(List(1,2,3)).headProxy
    // inferred: val x: Forcible[Int] = new DeliteCollection[Int](List.apply[Int](1, 2, 3)).headProxy[Forcible[Int]](forcibleInt);

    val xAlready = new DeliteCollection(List(DeliteInt(1),DeliteInt(2),DeliteInt(3))).headProxy
    // inferred: val xAlready: DeliteInt = new DeliteCollection[DeliteInt](List.apply[DeliteInt](DeliteInt(1), DeliteInt(2), DeliteInt(3))).headProxy[DeliteInt](trivial[DeliteInt]);
  }
}