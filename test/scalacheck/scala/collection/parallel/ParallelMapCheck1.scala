package scala.collection.parallel



import org.scalacheck.Prop._





abstract class ParallelMapCheck[K, V](collname: String) extends ParallelIterableCheck[(K, V)](collname) {
  type CollType <: ParMap[K, V]

  property("gets iterated keys") = forAllNoShrink(collectionPairs) {
    case (t, coll) =>
    val containsT = for ((k, v) <- t) yield (coll.get(k) == Some(v))
    val containsSelf = coll.map { case (k, v) => coll.get(k) == Some(v) }
    ("Par contains elements of seq map" |: containsT.forall(_ == true)) &&
    ("Par contains elements of itself" |: containsSelf.forall(_ == true))
  }

}







































