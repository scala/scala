package scala.collection.parallel



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.collection._
import scala.collection.parallel._




abstract class ParallelMapCheck[K, V](collname: String) extends ParallelIterableCheck[(K, V)](collname) {
  type CollType <: ParMap[K, V]
  
  property("gets iterated keys") = forAll(collectionPairs) {
    case (t, coll) =>
    val containsT = for ((k, v) <- t) yield (coll.get(k) == Some(v))
    val containsSelf = for ((k, v) <- coll) yield (coll.get(k) == Some(v))
    ("Par contains elements of seq map" |: containsT.forall(_ == true)) &&
    ("Par contains elements of itself" |: containsSelf.forall(_ == true))
  }  
   
}







































