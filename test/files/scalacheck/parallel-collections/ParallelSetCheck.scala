package scala.collection.parallel



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.collection._
import scala.collection.parallel._




abstract class ParallelSetCheck[T](collname: String) extends ParallelIterableCheck[T](collname) {
  type CollType <: ParSet[T]
  
  property("gets iterated keys") = forAll(collectionPairs) {
    case (t, coll) =>
    val containsT = for (elem <- t) yield (coll.contains(elem))
    val containsSelf = for (elem <- coll) yield (coll.contains(elem))
    ("Par contains elements of seq map" |: containsT.forall(_ == true)) &&
    ("Par contains elements of itself" |: containsSelf.forall(_ == true))
  }  
   
}


































