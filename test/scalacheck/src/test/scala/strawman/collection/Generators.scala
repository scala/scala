package strawman.collection

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.listOf

import scala.collection.immutable.{List => ScalaList}
import strawman.collection.immutable.List
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  implicit def arbitraryList[A](implicit arbitraryOldList: Arbitrary[ScalaList[A]]): Arbitrary[List[A]] =
    Arbitrary(arbitraryOldList.arbitrary.map(scalaList => List.from(scalaList.toStrawman)))

  implicit def arbitraryMutableSeq[A](implicit arbitraryOldMutableSeq: Arbitrary[scala.collection.mutable.Seq[A]]): Arbitrary[mutable.Seq[A]] =
    Arbitrary(arbitraryOldMutableSeq.arbitrary.map(seq => mutable.Seq.from(seq.toStrawman)))

  implicit def arbitraryMutableMap[K, V](implicit arbitraryOldMutableMap: Arbitrary[scala.collection.mutable.Map[K, V]]): Arbitrary[mutable.Map[K, V]] =
    Arbitrary(arbitraryOldMutableMap.arbitrary.map(m => mutable.Map.from(m.toSeq.toStrawman)))

  implicit def arbitraryMutableSet[A](implicit arbitraryOldMutableSet: Arbitrary[scala.collection.mutable.Set[A]]): Arbitrary[mutable.Set[A]] =
    Arbitrary(arbitraryOldMutableSet.arbitrary.map(m => mutable.Set.from(m.toSeq.toStrawman)))

  def genTreeSet[A: Arbitrary: Ordering]: Gen[mutable.TreeSet[A]] =
    for {
      elements <- listOf(arbitrary[A])
    } yield mutable.TreeSet(elements: _*)
  implicit def arbTreeSet[A : Arbitrary : Ordering]: Arbitrary[mutable.TreeSet[A]] = Arbitrary(genTreeSet)

  implicit def arbitraryImmutableHashSet[A](implicit arbitraryOldMutableSet: Arbitrary[scala.collection.mutable.Set[A]]): Arbitrary[immutable.HashSet[A]] =
    Arbitrary(arbitraryOldMutableSet.arbitrary.map(m => immutable.HashSet.from(m.toSeq.toStrawman)))

  implicit def arbitraryImmutableChampHashSet[A](implicit arbitraryOldMutableSet: Arbitrary[scala.collection.mutable.Set[A]]): Arbitrary[immutable.ChampHashSet[A]] =
    Arbitrary(arbitraryOldMutableSet.arbitrary.map(m => immutable.ChampHashSet.from(m.toSeq.toStrawman)))

  implicit def arbitraryImmutableHashMap[K, V](implicit arbitraryOldMutableSet: Arbitrary[scala.collection.mutable.Map[K, V]]): Arbitrary[immutable.HashMap[K, V]] =
    Arbitrary(arbitraryOldMutableSet.arbitrary.map(m => immutable.HashMap.from(m.toSeq.toStrawman)))

  implicit def arbitraryImmutableChampHashMap[K, V](implicit arbitraryOldMutableSet: Arbitrary[scala.collection.mutable.Map[K, V]]): Arbitrary[immutable.ChampHashMap[K, V]] =
    Arbitrary(arbitraryOldMutableSet.arbitrary.map(m => immutable.ChampHashMap.from(m.toSeq.toStrawman)))

}
