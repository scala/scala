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
  def hasStrictOrder: Boolean

  def printDataStructureDebugInfo(cf: AnyRef) {
    // can be overridden in subclasses
  }

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

  property("reductions must be equal for assoc. operators") = forAll(collectionPairs) { case (t, coll) =>
    if (t.size != 0) {
      val results = for ((op, ind) <- reduceOperators.zipWithIndex) yield {
        ("op index: " + ind) |: t.reduceLeft(op) == coll.reduce(op)
      }
      results.reduceLeft(_ && _)
    } else "has size 0" |: true
  }

  property("counts must be equal") = forAll(collectionPairs) { case (t, coll) =>
    val results = for ((pred, ind) <- countPredicates.zipWithIndex) yield {
      val tc = t.count(pred)
      val cc = coll.count(pred)
      if (tc != cc) {
        println("from: " + t)
        println("and: " + coll.toList)
        println(tc)
        println(cc)
      }
      ("op index: " + ind) |: tc == cc
    }
    results.reduceLeft(_ && _)
  }

  property("forall must be equal") = forAll(collectionPairs) { case (t, coll) =>
    val results = for ((pred, ind) <- forallPredicates.zipWithIndex)
      yield ("op index: " + ind) |: t.forall(pred) == coll.forall(pred)
    results.reduceLeft(_ && _)
  }

  property("exists must be equal") = forAll(collectionPairs) { case (t, coll) =>
    val results = for ((pred, ind) <- existsPredicates.zipWithIndex)
      yield ("op index: " + ind) |: t.exists(pred) == coll.exists(pred)
    results.reduceLeft(_ && _)
  }

  property("both must find or not find an element") = forAll(collectionPairs) { case (t, coll) =>
    val results = for ((pred, ind) <- findPredicates.zipWithIndex) yield {
      val ft = t.find(pred)
      val fcoll = coll.find(pred)
      ("op index: " + ind) |: ((ft == None && fcoll == None) || (ft != None && fcoll != None))
    }
    results.reduceLeft(_ && _)
  }

  def areEqual(t1: Traversable[T], t2: Traversable[T]) = if (hasStrictOrder) {
    t1 == t2
  } else (t1, t2) match { // it is slightly delicate what `equal` means if the order is not strict
    case (m1: Map[_, _], m2: Map[_, _]) => m1 == m2
    case (i1: Iterable[_], i2: Iterable[_]) => i1.toSet == i2.toSet
    case _ => t1 == t2
  }

  property("mappings must be equal") = forAll(collectionPairs) { case (t, coll) =>
    val results = for ((f, ind) <- mapFunctions.zipWithIndex) yield {
      val ms = t.map(f)
      val mp = coll.map(f)
      if (!areEqual(ms, mp)) {
        println(t)
        println(coll)
        println("mapped to: ")
        println(ms)
        println(mp)
      }
      ("op index: " + ind) |: areEqual(ms, mp)
    }
    results.reduceLeft(_ && _)
  }

  property("collects must be equal") = forAll(collectionPairs) { case (t, coll) =>
    val results = for ((f, ind) <- partialMapFunctions.zipWithIndex) yield {
      val ps = t.collect(f)
      val pp = coll.collect(f)
      if (!areEqual(ps, pp)) {
        println(t)
        println(coll)
        println("collected to: ")
        println(ps)
        println(pp)
      }
      ("op index: " + ind) |: areEqual(ps, pp)
    }
    results.reduceLeft(_ && _)
  }

  property("flatMaps must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((f, ind) <- flatMapFunctions.zipWithIndex)
      yield ("op index: " + ind) |: areEqual(t.flatMap(f), coll.flatMap(f))).reduceLeft(_ && _)
  }

  property("filters must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((p, ind) <- filterPredicates.zipWithIndex) yield {
      val tf = t.filter(p)
      val cf = coll.filter(p)
      if (tf != cf || cf != tf) {
        println(t)
        println(coll)
        println("filtered to:")
        println(cf)
        println(tf)
        println("tf == cf - " + (tf == cf))
        println("cf == tf - " + (cf == tf))
        printDataStructureDebugInfo(cf)
      }
      ("op index: " + ind) |: tf == cf && cf == tf
    }).reduceLeft(_ && _)
  }

  property("filterNots must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((p, ind) <- filterNotPredicates.zipWithIndex)
      yield ("op index: " + ind) |: t.filterNot(p) == coll.filterNot(p)).reduceLeft(_ && _)
  }

  if (!isCheckingViews) property("partitions must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((p, ind) <- partitionPredicates.zipWithIndex) yield {
      val tpart = t.partition(p)
      val cpart = coll.partition(p)
      if (tpart != cpart) {
        println("from: " + t)
        println("and: " + coll)
        println(cpart)
        println(tpart)
      }
      ("op index: " + ind) |: tpart == cpart
    }).reduceLeft(_ && _)
  }

  if (hasStrictOrder) property("takes must be equal") = forAll(collectionPairsWithLengths) { case (t, coll, n) =>
    ("take " + n + " elements") |: t.take(n) == coll.take(n)
  }

  if (hasStrictOrder) property("drops must be equal") = forAll(collectionPairsWithLengths) { case (t, coll, n) =>
    ("drop " + n + " elements") |: t.drop(n) == coll.drop(n)
  }

  if (hasStrictOrder) property("slices must be equal") = forAll(collectionPairsWith2Indices)
  { case (t, coll, fr, slicelength) =>
    val from = if (fr < 0) 0 else fr
    val until = if (from + slicelength > t.size) t.size else from + slicelength
    val tsl = t.slice(from, until)
    val collsl = coll.slice(from, until)
    if (tsl != collsl) {
      println("---------------------- " + from + ", " + until)
      println("from: " + t)
      println("and: " + coll)
      println(tsl)
      println(collsl)
      println("as list: " + collsl.toList)
      println(collsl.iterator.hasNext)
      println(collsl.iterator.next)
      println(collsl.iterator.hasNext)
      println(collsl.iterator.next)
      println(collsl.iterator.hasNext)
      println(collsl.iterator.next)
      println(collsl.iterator.hasNext)
    }
    ("slice from " + from + " until " + until) |: tsl == collsl
  }

  if (hasStrictOrder) property("splits must be equal") = forAll(collectionPairsWithLengths) { case (t, coll, n) =>
    val tspl = t.splitAt(n)
    val cspl = coll.splitAt(n)
    if (tspl != cspl) {
      println("at: " + n)
      println("from: " + t)
      println("and: " + coll)
      println(tspl)
      println(cspl)
    }
    ("splitAt " + n) |: tspl == cspl
  }

  if (hasStrictOrder) property("takeWhiles must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((pred, ind) <- takeWhilePredicates.zipWithIndex) yield {
      val tt = t.takeWhile(pred)
      val ct = coll.takeWhile(pred)
      if (tt != ct) {
        println("from: " + t)
        println("and: " + coll)
        println("taking while...")
        println(tt)
        println(ct)
      }
      ("operator " + ind) |: tt == ct
    }).reduceLeft(_ && _)
  }

  if (hasStrictOrder) property("spans must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((pred, ind) <- spanPredicates.zipWithIndex) yield {
      val tsp = t.span(pred)
      val csp = coll.span(pred)
      if (tsp != csp) {
        println("from: " + t)
        println("and: " + coll)
        println("span with predicate " + ind)
        println(tsp)
        println(csp)
        println("---------------------------------")
        println(coll.span(pred))
        println("---------------------------------")
      }
      ("operator " + ind) |: tsp == csp
    }).reduceLeft(_ && _)
  }

  if (hasStrictOrder) property("dropWhiles must be equal") = forAll(collectionPairs) { case (t, coll) =>
    (for ((pred, ind) <- dropWhilePredicates.zipWithIndex) yield {
      ("operator " + ind) |: t.dropWhile(pred) == coll.dropWhile(pred)
    }).reduceLeft(_ && _)
  }

  property("folds must be equal for assoc. operators") = forAll(collectionPairs) { case (t, coll) =>
    (for (((first, op), ind) <- foldArguments.zipWithIndex) yield {
      val tres = t.foldLeft(first)(op)
      val cres = coll.fold(first)(op)
      if (cres != tres) {
        println("from: " + t)
        println("and: " + coll)
        println("folds are: ")
        println(tres)
        println(cres)
      }
      ("operator " + ind) |: tres == cres
    }).reduceLeft(_ && _)
  }

  property("++s must be equal") = forAll(collectionTriplets) { case (t, coll, colltoadd) =>
    val toadd = colltoadd
    val tr = t ++ toadd.iterator
    val cr = coll ++ toadd.iterator
    if (areEqual(tr, cr)) {
      println("from: " + t)
      println("and: " + coll.iterator.toList)
      println("adding: " + toadd)
      println(tr.toList)
      println(cr.iterator.toList)
    }
    ("adding " |: areEqual(tr, cr)) &&
    (for ((trav, ind) <- (addAllTraversables).zipWithIndex) yield {
      val tadded = t ++ trav
      val cadded = coll ++ collection.parallel.mutable.ParArray(trav.toSeq: _*)
      if (!areEqual(tadded, cadded)) {
        println("----------------------")
        println("from: " + t)
        println("and: " + coll)
        println("adding: " + trav)
        println(tadded)
        println(cadded)
      }
      ("traversable " + ind) |: areEqual(tadded, cadded)
    }).reduceLeft(_ && _)
  }

  if (hasStrictOrder) property("copies to array must be equal") = forAll(collectionPairs) { case (t, coll) =>
    val tarr = newArray(t.size)
    val collarr = newArray(coll.size)
    t.copyToArray(tarr, 0, t.size)
    coll.copyToArray(collarr, 0, coll.size)
    if (tarr.toSeq != collarr.toSeq) {
      println("from: " + t)
      println("and: " + coll)
      println(tarr.toSeq)
      println(collarr.toSeq)
    }
    tarr.toSeq == collarr.toSeq
  }

}







































