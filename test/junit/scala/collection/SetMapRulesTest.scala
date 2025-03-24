/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.annotation.unused
import scala.collection.{mutable, immutable, concurrent}
import scala.jdk.CollectionConverters._

/**
  * Test that various set and map implementation conform to the following rules:
  *
  * 1. Unless explicitly noted otherwise, containment and equality for key elements (i.e. set elements and map keys) refers to the `equals` / `hashCode` contract.
  * 2. Immutable updates of mutable collections *must* always return a unique copy without observable structural sharing, even when the collection is unchanged or the result is empty.
  * 3. Immutable updates of immutable collections *should* use structural sharing and avoid copying.
  * 4. When updating a collection (immutably or in place), key element identities already in the source collection (i.e. the receiver of a method call) *must* always be preferred over new key elements which are equal but have different identities.
  * 5. When transforming a collection (immutably or in place) into a new collection (e.g. `map`, `flatMap`, `collect`), only the transformation results are used. Existing key elements *must not* be preserved.
  * 6. Map values *must* always be updated (i.e. a new value wins over an existing value with a different identity)
  * 7. In immutable maps structural sharing *must not* be done for non-identical values; equality of map values is irrelevant.
  *
  * 1 is indirectly tested by using the `Value` instances as keys in all tests. 3 cannot be tested directly. The other
  * rules have explicit tests.
  */
@RunWith(classOf[JUnit4])
class SetMapRulesTest {

  class Value private (val id: Int, extra: Int) {
    override def hashCode: Int = id
    override def equals(o: Any): Boolean = o match {
      case v: Value => id == v.id
      case _ => false
    }
    override def toString: String = s"$id.$extra"
    def + (i: Int): Value = Value(id, extra + i)
    def toTuple: (Int, Int) = (id, extra)
    def incrementExtra = new Value(id, extra + 1)
  }
  object Value {
    private[this] val cache = new mutable.HashMap[(Int, Int), Value]
    def apply(id: Int, extra: Int): Value = cache.get((id, extra)) match {
      case Some(v) => v
      case None =>
        val v = new Value(id, extra)
        cache.put((id, extra), v)
        v
    }
  }

  implicit val valueOrdering: Ordering[Value] = Ordering.Int.on(_.id)

  val mapdata: Seq[Seq[(Value, Value)]] = 1.to(20).map(n => (Value(n, 1), Value(100+n, 1))).inits.toSeq.reverse
  val setdata: Seq[Seq[Value]] = 1.to(20).map(n => Value(n, 1)).inits.toSeq.reverse

  private def checkUnique[T <: mutable.Iterable[_]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val v2 = f(v1)
    assertNotSame(s"$op should be a different object", v1, v2)
  }

  private def checkPreservesKeyIdentities[T <: collection.Map[Value, Value]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val keys1 = v1.keysIterator.map(_.toTuple).toSet
    val v2 = f(v1)
    val keys2 = v2.keysIterator.map(_.toTuple).toSet
    assertTrue(s"$op should preserve key identities (all of $keys1 should be in $keys2)", keys1.forall(k => keys2.contains(k)))
  }

  private def checkPreservesIdentities[T <: collection.Iterable[Value]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val keys1 = v1.iterator.map(_.toTuple).toSet
    val v2 = f(v1)
    val keys2 = v2.iterator.map(_.toTuple).toSet
    assertTrue(s"$op should preserve key identities (all of $keys1 should be in $keys2)", keys1.forall(k => keys2.contains(k)))
  }

  private def checkDiscardsKeyIdentities[T <: collection.Map[Value, Value]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val keys1 = v1.keysIterator.map(_.toTuple).toSet
    val v2 = f(v1)
    val keys2 = v2.keysIterator.map(_.toTuple).toSet
    assertTrue(s"$op should discard key identities (none of $keys1 should be in $keys2)", keys1.forall(k => !keys2.contains(k)))
  }

  private def checkDiscardsIdentities[T <: collection.Iterable[Value]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val keys1 = v1.iterator.map(_.toTuple).toSet
    val v2 = f(v1)
    val keys2 = v2.iterator.map(_.toTuple).toSet
    assertTrue(s"$op should discard key identities (none of $keys1 should be in $keys2)", keys1.forall(k => !keys2.contains(k)))
  }

  private def checkAllValuesUpdated[T <: collection.Map[Value, Value]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val values1 = v1.valuesIterator.map(_.toTuple).toSet
    val v2 = f(v1)
    val values2 = v2.valuesIterator.map(_.toTuple).toSet
    assertTrue(s"$op should update values (none of $values1 should be in $values2)", values1.forall(k => !values2.contains(k)))
  }

  private def checkNoSharedValues[T <: immutable.Map[Value, Value]](gen: () => T, op: String)(f: T => T): Unit = {
    val v1 = gen()
    val entries1a = v1.iterator.map { case (k, v) => (k.toTuple, v.toTuple) }.toSet
    val v2 = f(v1)
    val entries1b = v1.iterator.map { case (k, v) => (k.toTuple, v.toTuple) }.toSet
    @unused val entries2 = v2.iterator.map { case (k, v) => (k.toTuple, v.toTuple) }.toSet
    assertEquals(s"$op should preserve original values ($entries1a should be equal to $entries1b)", entries1a, entries1b)
  }

  private def checkMap(gen: () => collection.Map[Value, Value]): Unit = {
    checkPreservesKeyIdentities(gen, "concat (identical value)")(_.concat(Seq((Value(1,2), Value(101,1)))))
    checkPreservesKeyIdentities(gen, "concat (equal value)")(_.concat(Seq((Value(1,2), Value(101,2)))))
    checkDiscardsKeyIdentities(gen, "map")(_.map { case (k, v) => (k + 1, v)})
    checkDiscardsKeyIdentities(gen, "flatMap")(_.flatMap { case (k, v) => Seq((k + 1, v))})
  }

  private def checkMutableMap(gen: () => mutable.Map[Value, Value]): Unit = {
    checkMap(gen)
    checkUnique(gen, "map")(_.map { case (k, v) => (k, v + 1) })
    checkUnique(gen, "map (identity)")(_.map(identity))
    checkUnique(gen, "filter (keep all)")(_.filter(_ => true))
    checkUnique(gen, "filter (drop all)")(_.filter(_ => false))
    checkUnique(gen, "flatMap")(_.flatMap(x => Iterable(x)))
    checkUnique(gen, "flatMap (to empty)")(_.flatMap(x => Nil))
    checkPreservesKeyIdentities(gen, "addOne")(_.addOne((Value(1,2), Value(101,2))))
    checkPreservesKeyIdentities(gen, "addAll")(_.addAll(Seq((Value(1,2), Value(101,2)))))
    checkPreservesKeyIdentities(gen, "update") { c => c.update(Value(1,2), Value(101,2)); c }
    checkPreservesKeyIdentities(gen, "put") { c => c.put(Value(1,2), Value(101,2)); c }
    checkAllValuesUpdated(gen, "addOne (identical key)") { c => c.addOne((Value(1,1), Value(101,2))).filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "addOne (equal key)") { c => c.addOne((Value(1,2), Value(101,2))).filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "addAll (identical key)") { c => c.addAll(Seq((Value(1,1), Value(101,2)))).filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "addAll (equal key)") { c => c.addAll(Seq((Value(1,2), Value(101,2)))).filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "update (identical key)") { c => c.update(Value(1,1), Value(101,2)); c.filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "update (equal key)") { c => c.update(Value(1,2), Value(101,2)); c.filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "put (identical key)") { c => c.put(Value(1,1), Value(101,2)); c.filter(_._1.id == 1) }
    checkAllValuesUpdated(gen, "put (equal key)") { c => c.put(Value(1,2), Value(101,2)); c.filter(_._1.id == 1) }
  }

  private def checkImmutableMap(gen: () => immutable.Map[Value, Value]): Unit = {
    checkMap(gen)
    checkPreservesKeyIdentities(gen, "updated (identical value)")(_.updated(Value(1,2), Value(101,1)))
    checkPreservesKeyIdentities(gen, "updated (equal value)")(_.updated(Value(1,2), Value(101,2)))
    checkPreservesKeyIdentities(gen, "+ (identical value)")(_.+((Value(1,2), Value(101,1))))
    checkPreservesKeyIdentities(gen, "+ (equal value)")(_.+((Value(1,2), Value(101,2))))

    val values = Seq((Value(1,2), Value(101,2)))
    val valuesSameCollection = gen().take(0).concat(values)
    for (vs <- Seq(values, valuesSameCollection)) {
      checkPreservesKeyIdentities(gen, "concat (identical key)")(_.concat(vs))
    }

    checkAllValuesUpdated(gen, "updated (identical key)")(_.updated(Value(1,1), Value(101,2)).filter(_._1.id == 1))
    checkAllValuesUpdated(gen, "updated (equal key)")(_.updated(Value(1,2), Value(101,2)).filter(_._1.id == 1))
    checkAllValuesUpdated(gen, "+ (identical key)")(_.+((Value(1,1), Value(101,2))).filter(_._1.id == 1))
    checkAllValuesUpdated(gen, "+ (equal key)")(_.+((Value(1,2), Value(101,2))).filter(_._1.id == 1))
    checkAllValuesUpdated(gen, "concat (identical key)")(_.concat(Seq((Value(1,1), Value(101,2)))).filter(_._1.id == 1))
    checkAllValuesUpdated(gen, "concat (equal key)")(_.concat(Seq((Value(1,2), Value(101,2)))).filter(_._1.id == 1))
    checkNoSharedValues(gen, "updated")(_.updated(Value(1,1), Value(101,2)))
    checkNoSharedValues(gen, "+")(_.+((Value(1,1), Value(101,2))))
    checkNoSharedValues(gen, "concat")(_.concat(Seq((Value(1,1), Value(101,2)))))
    checkNoSharedValues(gen, "map")(_.map { case (k, v) => (k, v + 1)})
    checkNoSharedValues(gen, "flatMap")(_.flatMap { case (k, v) => Seq((k, v + 1))})
  }

  private def checkSet(gen: () => collection.Set[Value]): Unit = {
    checkDiscardsIdentities(gen, "map")(_.map(_ + 1))
    checkDiscardsIdentities(gen, "flatMap")(_.flatMap(k => Seq(k + 1)))
  }

  private def checkMutableSet(gen: () => mutable.Set[Value]): Unit = {
    checkSet(gen)
    checkUnique(gen, "map (identity)")(_.map(identity))
    checkUnique(gen, "filter (keep all)")(_.filter(_ => true))
    checkUnique(gen, "filter (drop all)")(_.filter(_ => false))
    checkUnique(gen, "flatMap")(_.flatMap(x => Iterable(x)))
    checkUnique(gen, "flatMap (to empty)")(_.flatMap(x => Nil))
    checkPreservesIdentities(gen, "add") { c => c.add(Value(1,2)); c }
    checkPreservesIdentities(gen, "addOne")(_.addOne(Value(1,2)))
    checkPreservesIdentities(gen, "addAll")(_.addAll(Seq(Value(1,2))))
    checkPreservesIdentities(gen, "update") { c => c.update(Value(1,2), included = true); c }
  }

  private def checkImmutableSet(gen: () => immutable.Set[Value]): Unit = {
    checkSet(gen)
    checkPreservesIdentities(gen, "incl")(_.incl(Value(1,2)))

    val values = Seq(Value(1,2))
    val valuesSameCollection = gen().take(0).concat(values)
    for (vs <- Seq(values, valuesSameCollection)) {
      checkPreservesIdentities(gen, "concat")(_.concat(vs))
    }

    for (xs <- Seq(gen().take(0).concat(gen().map(_.incrementExtra)), Set.from(gen().map(_.incrementExtra)))) {
      checkPreservesIdentities(gen, "intersect")(_.intersect(xs))
    }
  }

  // Immutable maps

  @Test def testImmutableMap(): Unit =
    mapdata.foreach(d => checkImmutableMap(() => immutable.Map.from(d)))

  @Test def testImmutableListMap(): Unit =
    mapdata.foreach(d => checkImmutableMap(() => immutable.ListMap.from(d)))

  @Test def testImmutableVectorMap(): Unit =
    mapdata.foreach(d => checkImmutableMap(() => immutable.VectorMap.from(d)))

  @Test def testImmutableTreeMap(): Unit =
    mapdata.foreach(d => checkImmutableMap(() => immutable.TreeMap.from(d)))

  @Test def testImmutableHashMap(): Unit =
    mapdata.foreach(d => checkImmutableMap(() => immutable.HashMap.from(d)))

  // Mutable maps

  @Test def testMutableMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.Map.from(d)))

  @Test def testMutableHashMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.HashMap.from(d)))

  @deprecated("Uses OpenHashMap", since="2.13")
  @Test def testMutableOpenHashMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.OpenHashMap.from(d)))

  @annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
  @Test def testMutableAnyRefMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.AnyRefMap.from(d)))

  @Test def testMutableTreeMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.TreeMap.from(d)))

  @Test def testMutableLinkedHashMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.LinkedHashMap.from(d)))

  @Test def testMutableSeqMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.SeqMap.from(d)))

  @deprecated("Uses ListMap", since="2.13")
  @Test def testMutableListMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => mutable.ListMap.from(d)))

  @Test def testConcurrentTrieMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => concurrent.TrieMap.from(d)))

  @Test def testJavaHashMap(): Unit =
    mapdata.foreach(d => checkMutableMap(() => new java.util.HashMap[Value, Value].asScala.addAll(d)))

  // Immutable sets

  @Test def testImmutableSet(): Unit =
    setdata.foreach(d => checkImmutableSet(() => immutable.Set.from(d)))

  @Test def testImmutableHashSet(): Unit =
    setdata.foreach(d => checkImmutableSet(() => immutable.HashSet.from(d)))

  @Test def testImmutableListSet(): Unit =
    setdata.foreach(d => checkImmutableSet(() => immutable.ListSet.from(d)))

  @Test def testImmutableTreeSet(): Unit =
    setdata.foreach(d => checkImmutableSet(() => immutable.TreeSet.from(d)))

  // Mutable sets

  @Test def testMutableSet(): Unit =
    setdata.foreach(d => checkMutableSet(() => mutable.Set.from(d)))

  @Test def testMutableHashSet(): Unit =
    setdata.foreach(d => checkMutableSet(() => mutable.HashSet.from(d)))

  @Test def testMutableLinkedHashSet(): Unit =
    setdata.foreach(d => checkMutableSet(() => mutable.LinkedHashSet.from(d)))

  @Test def testMutableTreeSet(): Unit =
    setdata.foreach(d => checkMutableSet(() => mutable.TreeSet.from(d)))

  @Test def testJavaHashSet(): Unit =
    setdata.foreach(d => checkMutableSet(() => new java.util.HashSet[Value].asScala.addAll(d)))
}
