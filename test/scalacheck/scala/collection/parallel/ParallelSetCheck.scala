package scala.collection.parallel



import org.scalacheck.Prop._





abstract class ParallelSetCheck[T](collname: String) extends ParallelIterableCheck[T](collname) {
  type CollType <: ParSet[T]

  property("gets iterated keys") = forAllNoShrink(collectionPairs) {
    case (t, coll) =>
    val containsT = for (elem <- t) yield (coll.contains(elem))
    val containsSelf = for (elem <- coll) yield (coll.contains(elem))
    ("Par contains elements of seq map" |: containsT.forall(_ == true)) &&
    ("Par contains elements of itself" |: containsSelf.forall(_ == true))
  }

}


































