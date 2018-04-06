package strawman.collection

import org.scalacheck.Arbitrary.arbitrary

import scala.collection.immutable.{List => ScalaList, Stream => ScalaStream, Vector => ScalaVector}
import strawman.collection.immutable.List
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  def arbitraryCollection[A, C](implicit
    factory: Factory[A, C],
    a: Arbitrary[A]
  ): Arbitrary[C] =
    Arbitrary(
      for (as <- Gen.listOf(arbitrary[A])) yield {
        val b = factory.newBuilder()
        as.foreach(a => b += a)
        b.result()
      }
    )

  // Help type inference a bit…

  implicit def arbitraryIterable[A, CC[X] <: Iterable[X]](implicit
    factory: Factory[A, CC[A]],
    a: Arbitrary[A]
  ): Arbitrary[CC[A]] =
    arbitraryCollection

  implicit def arbitraryMap[K, V, CC[X, Y] <: Map[X, Y]](implicit
    factory: Factory[(K, V), CC[K, V]],
    k: Arbitrary[K],
    v: Arbitrary[V]
  ): Arbitrary[CC[K, V]] =
    arbitraryCollection

}
