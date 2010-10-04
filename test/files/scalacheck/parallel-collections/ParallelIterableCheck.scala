package scala.collection.parallel



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.collection._
import scala.collection.parallel._




abstract class ParallelIterableCheck[T](collName: String) extends Properties(collName) with Operators[T] {
  type CollType <: ParIterable[T] with Sequentializable[T, Iterable[T]]

  def values: Seq[Gen[T]]
  def instances(valgens: Seq[Gen[T]]): Gen[Traversable[T]]
  def fromTraversable(t: Traversable[T]): CollType
  def isCheckingViews: Boolean

  val rnd = new scala.util.Random

  def sample(gen: Gen[T]): T = {
    var s = gen.sample
    while (s == None) s = gen.sample
    s.get
  }

  def sampleValue: T = sample(values(rnd.nextInt(values.length)))

  def collectionPairs = for (inst <- instances(values)) yield (inst, fromTraversable(inst))

  def collectionPairsWithLengths = for (inst <- instances(values); s <- choose(0, inst.size))
    yield (inst, fromTraversable(inst), s)

  def collectionPairsWith2Indices = for (
      inst <- instances(values);
      f <- choose(0, inst.size);
      s <- choose(0, inst.size))
    yield (inst, fromTraversable(inst), f, s)

  def collectionTriplets = for (inst <- instances(values);
      updStart <- choose(0, inst.size); howMany <- choose(0, inst.size)) yield {
    val modif = inst.toSeq.patch(updStart, inst.toSeq, howMany)
    (inst, fromTraversable(inst), modif)
  }

  // property("reductions must be equal for assoc. operators") = forAll(collectionPairs) { case (t, coll) =>
  //   if (t.size != 0) {
  //     val results = for ((op, ind) <- reduceOperators.zipWithIndex) yield {
  //       ("op index: " + ind) |: t.reduceLeft(op) == coll.reduce(op)
  //     }
  //     results.reduceLeft(_ && _)
  //   } else "has size 0" |: true
  // }

  // property("counts must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   val results = for ((pred, ind) <- countPredicates.zipWithIndex) yield {
  //     val tc = t.count(pred)
  //     val cc = coll.count(pred)
  //     if (tc != cc) {
  //       println("from: " + t)
  //       println("and: " + coll.toList)
  //       println(tc)
  //       println(cc)
  //     }
  //     ("op index: " + ind) |: tc == cc
  //   }
  //   results.reduceLeft(_ && _)
  // }

  // property("forall must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   val results = for ((pred, ind) <- forallPredicates.zipWithIndex)
  //     yield ("op index: " + ind) |: t.forall(pred) == coll.forall(pred)
  //   results.reduceLeft(_ && _)
  // }

  // property("exists must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   val results = for ((pred, ind) <- existsPredicates.zipWithIndex)
  //     yield ("op index: " + ind) |: t.exists(pred) == coll.exists(pred)
  //   results.reduceLeft(_ && _)
  // }

  // property("both must find or not find an element") = forAll(collectionPairs) { case (t, coll) =>
  //   val results = for ((pred, ind) <- findPredicates.zipWithIndex) yield {
  //     val ft = t.find(pred)
  //     val fcoll = coll.find(pred)
  //     ("op index: " + ind) |: ((ft == None && fcoll == None) || (ft != None && fcoll != None))
  //   }
  //   results.reduceLeft(_ && _)
  // }

  // property("mappings must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   val results = for ((f, ind) <- mapFunctions.zipWithIndex)
  //     yield ("op index: " + ind) |: t.map(f) == coll.map(f)
  //   results.reduceLeft(_ && _)
  // }

  // property("collects must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   val results = for ((f, ind) <- partialMapFunctions.zipWithIndex) yield {
  //     val ps = t.collect(f)
  //     val pp = coll.collect(f)
  //     if (ps != pp) {
  //       println(t)
  //       println(coll)
  //       println("partially mapped to: ")
  //       println(ps)
  //       println(pp)
  //     }
  //     ("op index: " + ind) |: ps == pp
  //   }
  //   results.reduceLeft(_ && _)
  // }

  // property("flatMaps must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((f, ind) <- flatMapFunctions.zipWithIndex)
  //     yield ("op index: " + ind) |: t.flatMap(f) == coll.flatMap(f)).reduceLeft(_ && _)
  // }

  // property("filters must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((p, ind) <- filterPredicates.zipWithIndex)
  //     yield ("op index: " + ind) |: t.filter(p) == coll.filter(p)).reduceLeft(_ && _)
  // }

  // property("filterNots must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((p, ind) <- filterNotPredicates.zipWithIndex)
  //     yield ("op index: " + ind) |: t.filterNot(p) == coll.filterNot(p)).reduceLeft(_ && _)
  // }

  // if (!isCheckingViews) property("partitions must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((p, ind) <- partitionPredicates.zipWithIndex) yield {
  //     val tpart = t.partition(p)
  //     val cpart = coll.partition(p)
  //     if (tpart != cpart) {
  //       println("from: " + t)
  //       println("and: " + coll)
  //       println(cpart)
  //       println(tpart)
  //     }
  //     ("op index: " + ind) |: tpart == cpart
  //   }).reduceLeft(_ && _)
  // }

  // property("takes must be equal") = forAll(collectionPairsWithLengths) { case (t, coll, n) =>
  //   ("take " + n + " elements") |: t.take(n) == coll.take(n)
  // }

  // property("drops must be equal") = forAll(collectionPairsWithLengths) { case (t, coll, n) =>
  //   ("drop " + n + " elements") |: t.drop(n) == coll.drop(n)
  // }

  // property("slices must be equal") = forAll(collectionPairsWith2Indices)
  // { case (t, coll, fr, slicelength) =>
  //   val from = if (fr < 0) 0 else fr
  //   val until = if (from + slicelength > t.size) t.size else from + slicelength
  //   val tsl = t.slice(from, until)
  //   val collsl = coll.slice(from, until)
  //   if (tsl != collsl) {
  //     println("---------------------- " + from + ", " + until)
  //     println("from: " + t)
  //     println("and: " + coll)
  //     println(tsl)
  //     println(collsl)
  //     println("as list: " + collsl.toList)
  //     println(tsl.asInstanceOf[Seq[T]].sameElements(collsl))
  //     println(collsl.iterator.hasNext)
  //     println(collsl.iterator.next)
  //     println(collsl.iterator.hasNext)
  //     println(collsl.iterator.next)
  //     println(collsl.iterator.hasNext)
  //     println(collsl.iterator.next)
  //     println(collsl.iterator.hasNext)
  //   }
  //   ("slice from " + from + " until " + until) |: tsl == collsl
  // }

  // property("splits must be equal") = forAll(collectionPairsWithLengths) { case (t, coll, n) =>
  //   val tspl = t.splitAt(n)
  //   val cspl = coll.splitAt(n)
  //   if (tspl != cspl) {
  //     println("at: " + n)
  //     println("from: " + t)
  //     println("and: " + coll)
  //     println(tspl)
  //     println(cspl)
  //   }
  //   ("splitAt " + n) |: tspl == cspl
  // }

  // property("takeWhiles must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((pred, ind) <- takeWhilePredicates.zipWithIndex)
  //     yield ("operator " + ind) |: t.takeWhile(pred) == coll.takeWhile(pred)).reduceLeft(_ && _)
  // }

  // property("spans must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((pred, ind) <- spanPredicates.zipWithIndex) yield {
  //     val tsp = t.span(pred)
  //     val csp = coll.span(pred)
  //     if (tsp != csp) {
  //       println("from: " + t)
  //       println("and: " + coll)
  //       println(tsp)
  //       println(csp)
  //     }
  //     ("operator " + ind) |: tsp == csp
  //   }).reduceLeft(_ && _)
  // }

  // property("dropWhiles must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   (for ((pred, ind) <- dropWhilePredicates.zipWithIndex) yield {
  //     ("operator " + ind) |: t.dropWhile(pred) == coll.dropWhile(pred)
  //   }).reduceLeft(_ && _)
  // }

  // property("folds must be equal for assoc. operators") = forAll(collectionPairs) { case (t, coll) =>
  //   (for (((first, op), ind) <- foldArguments.zipWithIndex)
  //     yield ("operator " + ind) |: t.foldLeft(first)(op) == coll.fold(first)(op)).reduceLeft(_ && _)
  // }

  // property("++s must be equal") = forAll(collectionTriplets) { case (t, coll, colltoadd) =>
  //   val toadd = colltoadd
  //   val tr = t ++ toadd
  //   val cr = coll ++ fromTraversable(toadd).iterator
  //   if (!tr.toList.iterator.sameElements(cr.iterator)) {
  //     println("from: " + t)
  //     println("and: " + coll.iterator.toList)
  //     println("adding: " + toadd)
  //     println(tr.toList)
  //     println(cr.iterator.toList)
  //   }
  //   ("adding " |: tr == cr) &&
  //   (for ((trav, ind) <- (addAllTraversables).zipWithIndex) yield {
  //     val tadded = t ++ trav
  //     val cadded = coll ++ fromTraversable(trav.toList)
  //     if (tadded != cadded) {
  //       println("----------------------")
  //       println("from: " + t)
  //       println("and: " + coll.iterator.toList)
  //       println("adding: " + trav)
  //       println(tadded.toList)
  //       println(cadded.iterator.toList)
  //     }
  //     ("traversable " + ind) |: (tadded) == (cadded)
  //   }).reduceLeft(_ && _)
  // }

  // property("copies to array must be equal") = forAll(collectionPairs) { case (t, coll) =>
  //   val tarr = newArray(t.size)
  //   val collarr = newArray(coll.size)
  //   t.copyToArray(tarr, 0, t.size)
  //   coll.copyToArray(collarr, 0, coll.size)
  //   if (tarr.toSeq != collarr.toSeq) {
  //     println("from: " + t)
  //     println("and: " + coll)
  //     println(tarr.toSeq)
  //     println(collarr.toSeq)
  //   }
  //   tarr.toSeq == collarr.toSeq
  // }

}







































