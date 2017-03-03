package scala.collection.parallel



import org.scalacheck._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Properties

import scala.collection._
import scala.collection.parallel._





abstract class ParallelSeqCheck[T](collName: String) extends ParallelIterableCheck[T](collName) with SeqOperators[T] {

  type CollType <: collection.parallel.ParSeq[T]


  def ofSize(vals: Seq[Gen[T]], sz: Int): Seq[T]
  def fromSeq(s: Seq[T]): CollType

  override def instances(vals: Seq[Gen[T]]): Gen[Seq[T]] = oneOf(
    Gen.const(ofSize(vals, 1)),
    sized(
      sz =>
      ofSize(vals, sz)
    ),
    for (sz <- choose(1000, 2000)) yield ofSize(vals, sz)
  )


  def fromTraversable(t: Traversable[T]) = fromSeq(traversable2Seq(t))
  def traversable2Seq(t: Traversable[T]): Seq[T] = {
    if (t.isInstanceOf[Iterable[_]]) t.asInstanceOf[Iterable[T]].iterator.toList else t.toList
  }

  override def collectionPairs: Gen[(Seq[T], CollType)] = for (inst <- instances(values)) yield (inst, fromSeq(inst))

  override def collectionPairsWithLengths: Gen[(Seq[T], CollType, Int)] =
    for (inst <- instances(values); s <- choose(0, inst.size)) yield (inst, fromSeq(inst), s);

  def collectionPairsWithModifiedWithLengths: Gen[(Seq[T], CollType, ParSeq[T], Int)] =
    for (inst <- instances(values); s <- choose(0, inst.size);
    updateStart <- choose(0, inst.size); howMany <- choose(0, inst.size)) yield {
      val parcoll = fromSeq(inst)
      val parcollmodif = fromSeq(modifySlightly(inst, updateStart, howMany))
      (inst, parcoll, parcollmodif, s)
    }

  def collectionPairsWithModified: Gen[(Seq[T], CollType, ParSeq[T])] =
    for (inst <- instances(values); updateStart <- choose(0, inst.size); howMany <- choose(0, inst.size)) yield {
      val parcoll = fromSeq(inst)
      val parcollmodif = fromSeq(modifySlightly(inst, updateStart, howMany))
      (inst, parcoll, parcollmodif)
    }

  def collectionPairsWithSliced: Gen[(Seq[T], CollType, ParSeq[T])] =
    for (inst <- instances(values); sliceStart <- choose(0, inst.size); howMany <- choose(0, inst.size)) yield {
      val parcoll = fromSeq(inst)
      val parcollsliced = fromSeq(inst.slice(sliceStart, sliceStart + howMany))
      (inst, parcoll, parcollsliced)
    }

  def collectionTripletsWith2Indices: Gen[(Seq[T], CollType, Seq[T], Int, Int)] =
    for (inst <- instances(values); f <- choose(0, inst.size); s <- choose(0, inst.size - f);
      third <- instances(values); sliceStart <- choose(0, inst.size); howMany <- choose(0, inst.size)) yield {
      (inst, fromSeq(inst), inst.slice(sliceStart, sliceStart + howMany), f, s)
    }

  private def modifySlightly(coll: Seq[T], updateStart: Int, howMany: Int) = {
    coll.patch(updateStart, coll, howMany)
  }

  property("segmentLengths must be equal") = forAllNoShrink(collectionPairsWithLengths) { case (s, coll, len) =>
    (for ((pred, ind) <- segmentLengthPredicates.zipWithIndex) yield {
      val slen = s.segmentLength(pred, if (len < 0) 0 else len)
      val clen = coll.segmentLength(pred, len)
      if (slen != clen) {
        println("from: " + s)
        println("and: " + coll)
        println(slen)
        println(clen)
      }
      ("operator " + ind) |: slen == clen
    }).reduceLeft(_ && _)
  }

  property("prefixLengths must be equal") = forAllNoShrink(collectionPairs) { case (s, coll) =>
    (for ((pred, ind) <- segmentLengthPredicates.zipWithIndex) yield {
      ("operator " + ind) |: s.prefixLength(pred) == coll.prefixLength(pred)
    }).reduceLeft(_ && _)
  }

  property("indexWheres must be equal") = forAllNoShrink(collectionPairsWithLengths) { case (s, coll, len) =>
    (for ((pred, ind) <- indexWherePredicates.zipWithIndex) yield {
      val sind = s.indexWhere(pred, len)
      val cind = coll.indexWhere(pred, len)
      if (sind != cind) {
        println("from: " + s)
        println("and: " + coll)
        println("at: " + len)
        println(sind)
        println(cind)
      }
      ("operator " + ind) |: sind == cind
    }).reduceLeft(_ && _)
  }

  property("lastIndexWheres must be equal") = forAllNoShrink(collectionPairsWithLengths) { case (s, coll, len) =>
    (for ((pred, ind) <- lastIndexWherePredicates.zipWithIndex) yield {
      val end = if (len >= s.size) s.size - 1 else len
      val sind = s.lastIndexWhere(pred, end)
      val cind = coll.lastIndexWhere(pred, end)
      ("operator " + ind) |: sind == cind
    }).reduceLeft(_ && _)
  }

  property("reverses must be equal") = forAllNoShrink(collectionPairs) { case (s, coll) =>
    (s.length == 0 && s.getClass == classOf[collection.immutable.Range]) ||
    {
      val sr = s.reverse
      val cr = coll.reverse
      if (sr != cr) {
        println("from: " + s)
        println("and: " + coll)
        println(sr)
        println(cr)
      }
      sr == cr
    }
  }

  property("reverseMaps must be equal") = forAllNoShrink(collectionPairs) { case (s, coll) =>
    (for ((f, ind) <- reverseMapFunctions.zipWithIndex) yield {
      ("operator " + ind) |: s.reverseMap(f) == coll.reverseMap(f)
    }).reduceLeft(_ && _)
  }

  property("sameElements must be equal") = forAllNoShrink(collectionPairsWithModifiedWithLengths) {
  case (s, coll, collmodif, len) =>
    val pos = if (len < 0) 0 else len
    val scm = s.sameElements(collmodif)
    val ccm = coll.sameElements(collmodif)
    if (scm != ccm) {
      println("Comparing: " + s)
      println("and: " + coll)
      println("with: " + collmodif)
      println(scm)
      println(ccm)
    }
    ("Nil" |: s.sameElements(Nil) == coll.sameElements(Nil)) &&
    ("toList" |: s.sameElements(s.toList) == coll.sameElements(coll.toList)) &&
    ("identity" |: s.sameElements(s.map(e => e)) == coll.sameElements(coll.map(e => e))) &&
    ("vice-versa" |: s.sameElements(coll) == coll.sameElements(s)) &&
    ("equal" |: s.sameElements(coll)) &&
    ("modified" |: scm == ccm) &&
    (for ((it, ind) <- sameElementsSeqs.zipWithIndex) yield {
      val sres = s.sameElements(it)
      val pres = coll.sameElements(it)
      if (sres != pres) {
        println("Comparing: " + s)
        println("and: " + coll)
        println("with: " + it)
        println(sres)
        println(pres)
      }
      ("collection " + ind) |: sres == pres
    }).reduceLeft(_ && _)
  }

  property("startsWiths must be equal") = forAllNoShrink(collectionPairsWithModifiedWithLengths) {
  case (s, coll, collmodif, len) =>
    val pos = if (len < 0) 0 else len
    ("start with self" |: s.startsWith(s) == coll.startsWith(coll)) &&
    ("tails correspond" |: (s.length == 0 || s.startsWith(s.tail, 1) == coll.startsWith(coll.tail, 1))) &&
    ("with each other" |: coll.startsWith(s)) &&
    ("modified" |: s.startsWith(collmodif) == coll.startsWith(collmodif)) &&
    ("modified2" |: s.startsWith(collmodif, pos) == coll.startsWith(collmodif, pos)) &&
    (for (sq <- startEndSeqs) yield {
      val ss = s.startsWith(sq, pos)
      val cs = coll.startsWith(fromSeq(sq), pos)
      if (ss != cs) {
        println("from: " + s)
        println("and: " + coll)
        println("test seq: " + sq)
        println("from pos: " + pos)
        println(ss)
        println(cs)
        println(coll.iterator.psplit(pos, coll.length - pos)(1).toList)
      }
      ("seq " + sq) |: ss == cs
    }).reduceLeft(_ && _)
  }

  property("endsWiths must be equal") = forAllNoShrink(collectionPairsWithModified) {
  case (s, coll, collmodif) =>
    ("ends with self" |: s.endsWith(s) == coll.endsWith(s)) &&
    ("ends with tail" |: (s.length == 0 || s.endsWith(s.tail) == coll.endsWith(coll.tail))) &&
    ("with each other" |: coll.endsWith(s)) &&
    ("modified" |: s.startsWith(collmodif) == coll.endsWith(collmodif)) &&
    (for (sq <- startEndSeqs) yield {
      val sew = s.endsWith(sq)
      val cew = coll.endsWith(fromSeq(sq))
      if (sew != cew) {
        println("from: " + s)
        println("and: " + coll)
        println(sew)
        println(cew)
      }
      ("seq " + sq) |: sew == cew
    }).reduceLeft(_ && _)
  }

  property("unions must be equal") = forAllNoShrink(collectionPairsWithModified) { case (s, coll, collmodif) =>
    ("modified" |: s.union(collmodif.seq) == coll.union(collmodif)) &&
    ("empty" |: s.union(Nil) == coll.union(fromSeq(Nil)))
  }

  // This is failing with my views patch: array index out of bounds in the array iterator.
  // Couldn't see why this and only this was impacted, could use a second pair of eyes.
  //
  // This was failing because some corner cases weren't added to the patch method in ParSeqLike.
  // Curiously, this wasn't detected before.
  //
  if (!isCheckingViews) property("patches must be equal") = forAll(collectionTripletsWith2Indices) {
    case (s, coll, pat, from, repl) =>
    ("with seq" |: s.patch(from, pat, repl) == coll.patch(from, pat, repl)) &&
    ("with par" |: s.patch(from, pat, repl) == coll.patch(from, fromSeq(pat), repl)) &&
    ("with empty" |: s.patch(from, Nil, repl) == coll.patch(from, fromSeq(Nil), repl)) &&
    ("with one" |: (s.length == 0 || s.patch(from, List(s(0)), 1) == coll.patch(from, fromSeq(List(coll(0))), 1)))
  }

  if (!isCheckingViews) property("updates must be equal") = forAllNoShrink(collectionPairsWithLengths) { case (s, coll, len) =>
    val pos = if (len >= s.length) s.length - 1 else len
    if (s.length > 0) {
      val supd = s.updated(pos, s(0))
      val cupd = coll.updated(pos, coll(0))
      if (supd != cupd) {
        println("from: " + s)
        println("and: " + coll)
        println(supd)
        println(cupd)
      }
      "from first" |: (supd == cupd)
    } else "trivially" |: true
  }

  property("prepends must be equal") = forAllNoShrink(collectionPairs) { case (s, coll) =>
    s.length == 0 || s(0) +: s == coll(0) +: coll
  }

  property("appends must be equal") = forAllNoShrink(collectionPairs) { case (s, coll) =>
    s.length == 0 || s :+ s(0) == coll :+ coll(0)
  }

  property("padTos must be equal") = forAllNoShrink(collectionPairsWithLengths) { case (s, coll, len) =>
    val someValue = sampleValue
    val sdoub = s.padTo(len * 2, someValue)
    val cdoub = coll.padTo(len * 2, someValue)
    if (sdoub != cdoub) {
      println("from: " + s)
      println("and: " + coll)
      println(sdoub)
      println(cdoub)
    }
    ("smaller" |: s.padTo(len / 2, someValue) == coll.padTo(len / 2, someValue)) &&
      ("bigger" |: sdoub == cdoub)
  }

  property("corresponds must be equal") = forAllNoShrink(collectionPairsWithModified) { case (s, coll, modified) =>
    val modifcut = modified.toSeq.slice(0, modified.length)
    ("self" |: s.corresponds(s)(_ == _) == coll.corresponds(coll)(_ == _)) &&
    ("modified" |: s.corresponds(modified.seq)(_ == _) == coll.corresponds(modified)(_ == _)) &&
    ("modified2" |: s.corresponds(modifcut)(_ == _) == coll.corresponds(modifcut)(_ == _))
  }

}



















