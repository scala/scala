import java.io._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Try
import scala.collection.mutable.{RedBlackTree => RB}

package scala.collection.mutable {

  trait Generators {

    def genRedBlackTree[A: Arbitrary: Ordering, B: Arbitrary]: Gen[RB.Tree[A, B]] = {
      import org.scalacheck.Gen._
      for { entries <- listOf(arbitrary[(A, B)]) } yield {
        val tree = RB.Tree.empty[A, B]
        entries.foreach { case (k, v) => RB.insert(tree, k, v) }
        tree
      }
    }

    // Note: in scalacheck 1.12.2 tree maps can be automatically generated without the need for custom
    // machinery
    def genTreeMap[A: Arbitrary: Ordering, B: Arbitrary]: Gen[mutable.TreeMap[A, B]] = {
      import org.scalacheck.Gen._
      for {
        keys <- listOf(arbitrary[A])
        values <- listOfN(keys.size, arbitrary[B])
      } yield mutable.TreeMap(keys zip values: _*)
    }

    implicit def arbRedBlackTree[A: Arbitrary: Ordering, B: Arbitrary] = Arbitrary(genRedBlackTree[A, B])
    implicit def arbTreeMap[A: Arbitrary: Ordering, B: Arbitrary] = Arbitrary(genTreeMap[A, B])
  }

  object RedBlackTreeProperties extends Properties("mutable.RedBlackTree") with Generators {
    type K = String
    type V = Int

    property("initial invariants") = forAll { (tree: RB.Tree[K, V]) =>
      RB.isValid(tree)
    }

    property("insert") = forAll { (tree: RB.Tree[K, V], entries: Seq[(K, V)]) =>
      entries.foreach { case (k, v) => RB.insert(tree, k, v) }
      RB.isValid(tree) && entries.toMap.forall { case (k, v) => RB.get(tree, k) == Some(v) }
    }

    property("delete") = forAll { (tree: RB.Tree[K, V], ks: Seq[K]) =>
      ks.foreach { k => RB.delete(tree, k) }
      RB.isValid(tree) && ks.toSet.forall { k => RB.get(tree, k) == None }
    }

    property("insert & delete") = forAll { (tree: RB.Tree[K, V], ops: Seq[Either[(K, V), K]]) =>
      ops.foreach {
        case Left((k, v)) => RB.insert(tree, k, v)
        case Right(k) => RB.delete(tree, k)
      }
      RB.isValid(tree)
    }

    property("min") = forAll { (entries: Seq[(K, V)]) =>
      val tree = RB.Tree.empty[K, V]
      entries.foreach { case (k, v) => RB.insert(tree, k, v) }
      RB.min(tree) == (if (entries.isEmpty) None else Some(entries.toMap.min))
    }

    property("max") = forAll { (entries: Seq[(K, V)]) =>
      val tree = RB.Tree.empty[K, V]
      entries.foreach { case (k, v) => RB.insert(tree, k, v) }
      RB.max(tree) == (if (entries.isEmpty) None else Some(entries.toMap.max))
    }
  }

  object MutableTreeMapProperties extends Properties("mutable.TreeMap") with Generators {
    type K = String
    type V = Int

    property("get, contains") = forAll { (allEntries: Map[K, V]) =>
      val entries = allEntries.take(allEntries.size / 2)

      val map = mutable.TreeMap[K, V]()
      map ++= entries

      allEntries.forall { case (k, v) =>
        map.contains(k) == entries.contains(k) &&
          map.get(k) == entries.get(k)
      }
    }

    property("size, isEmpty") = forAll { (entries: Map[K, V]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries
      map.size == entries.size && map.isEmpty == entries.isEmpty
    }

    property("+=") = forAll { (map: mutable.TreeMap[K, V], k: K, v: V) =>
      val oldSize = map.size
      val containedKeyBefore = map.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize else oldSize + 1

      map += (k -> v)
      map.contains(k) && map.get(k) == Some(v) && map.size == newExpectedSize
    }

    property("++=") = forAll { (map: mutable.TreeMap[K, V], entries: Seq[(K, V)]) =>
      val oldEntries = map.toMap
      map ++= entries
      (oldEntries ++ entries).forall { case (k, v) => map.get(k) == Some(v) }
    }

    property("-=") = forAll { (map: mutable.TreeMap[K, V], k: K) =>
      val oldSize = map.size
      val containedKeyBefore = map.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize - 1 else oldSize

      map -= k
      !map.contains(k) && map.get(k) == None && map.size == newExpectedSize
    }

    property("--=") = forAll { (map: mutable.TreeMap[K, V], ks: Seq[K]) =>
      val oldElems = map.toList
      map --= ks
      val deletedElems = ks.toSet
      oldElems.forall { case (k, v) => map.get(k) == (if(deletedElems(k)) None else Some(v)) }
    }

    property("iterator") = forAll { (entries: Map[K, V]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      map.iterator.toSeq == entries.toSeq.sorted
    }

    property("iteratorFrom") = forAll { (entries: Map[K, V], k: K) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      map.iteratorFrom(k).toSeq == entries.filterKeys(_ >= k).toSeq.sorted
    }

    property("keysIteratorFrom") = forAll { (entries: Map[K, V], k: K) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      map.keysIteratorFrom(k).toSeq == entries.keysIterator.filter(_ >= k).toSeq.sorted
    }

    property("valuesIteratorFrom") = forAll { (entries: Map[K, V], k: K) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      map.valuesIteratorFrom(k).toSeq == entries.filterKeys(_ >= k).toSeq.sorted.map(_._2)
    }

    property("headOption") = forAll { (map: mutable.TreeMap[K, V]) =>
      map.headOption == Try(map.iterator.next()).toOption
    }

    property("lastOption") = forAll { (map: mutable.TreeMap[K, V]) =>
      map.lastOption == Try(map.iterator.max).toOption
    }

    property("clear") = forAll { (map: mutable.TreeMap[K, V]) =>
      map.clear()
      map.isEmpty && map.size == 0
    }

    property("serializable") = forAll { (map: mutable.TreeMap[K, V]) =>
      val bytesOut = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytesOut)
      out.writeObject(map)
      val bytes = bytesOut.toByteArray

      val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
      val sameMap = in.readObject().asInstanceOf[mutable.TreeMap[K, V]]
      map.iterator.toSeq == sameMap.iterator.toSeq
    }

    property("same behavior as immutable.TreeMap") = forAll { ops: Seq[Either[(K, V), K]] =>
      var imap = immutable.TreeMap[K, V]()
      val mmap = mutable.TreeMap[K, V]()

      ops.foreach {
        case Left((k, v)) => imap += k -> v; mmap += k -> v
        case Right(k) => imap -= k; mmap -= k
      }

      imap.toList == mmap.toList
    }
  }

  object MutableTreeMapViewProperties extends Properties("mutable.TreeMapView") with Generators {
    type K = String
    type V = Int

    implicit val ord = implicitly[Ordering[K]]

    def in(key: K, from: Option[K], until: Option[K]) =
      from.fold(true)(_ <= key) && until.fold(true)(_ > key)

    def entriesInView[This <: TraversableOnce[(K, V)], That](entries: This, from: Option[K], until: Option[K])(implicit bf: CanBuildFrom[This, (K, V), That]) = {
      (bf.apply(entries) ++= entries.filter { case (k, _) => in(k, from, until) }).result()
    }

    property("get, contains") = forAll { (allEntries: Map[K, V], from: Option[K], until: Option[K]) =>
      val entries = allEntries.take(allEntries.size / 2)

      val map = mutable.TreeMap[K, V]()
      map ++= entries

      val mapView = map.rangeImpl(from, until)
      allEntries.forall { case (k, v) =>
        mapView.contains(k) == (in(k, from, until) && entries.contains(k)) &&
          mapView.get(k) == (if(in(k, from, until)) entries.get(k) else None)
      }
    }

    property("size, isEmpty") = forAll { (entries: Map[K, V], from: Option[K], until: Option[K]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      val mapView = map.rangeImpl(from, until)
      mapView.size == entriesInView(entries, from, until).size &&
        mapView.isEmpty == !entries.exists { kv => in(kv._1, from, until) }
    }

    property("+=") = forAll { (map: mutable.TreeMap[K, V], k: K, v: V, from: Option[K], until: Option[K]) =>
      val oldSize = map.size
      val containedKeyBefore = map.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize else oldSize + 1
      val isInRange = in(k, from, until)

      val mapView = map.rangeImpl(from, until)
      mapView += (k -> v)

      map.contains(k) && map.get(k) == Some(v) && map.size == newExpectedSize &&
        mapView.contains(k) == isInRange &&
        mapView.get(k) == (if(isInRange) Some(v) else None)
    }

    property("++=") = forAll { (map: mutable.TreeMap[K, V], entries: Seq[(K, V)], from: Option[K], until: Option[K]) =>
      val mapView = map.rangeImpl(from, until)
      mapView ++= entries
      entries.toMap.forall { case (k, v) =>
        map.get(k) == Some(v) &&
          mapView.get(k) == (if (in(k, from, until)) Some(v) else None)
      }
    }

    property("-=") = forAll { (map: mutable.TreeMap[K, V], k: K, from: Option[K], until: Option[K]) =>
      val oldSize = map.size
      val containedKeyBefore = map.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize - 1 else oldSize

      val mapView = map.rangeImpl(from, until)
      mapView -= k

      !map.contains(k) && map.get(k) == None && map.size == newExpectedSize &&
        !mapView.contains(k) &&
        mapView.get(k) == None
    }

    property("--=") = forAll { (map: mutable.TreeMap[K, V], ks: Seq[K], from: Option[K], until: Option[K]) =>
      val mapView = map.rangeImpl(from, until)
      mapView --= ks
      ks.toSet.forall { k => map.get(k) == None && mapView.get(k) == None }
    }

    property("iterator") = forAll { (entries: Map[K, V], from: Option[K], until: Option[K]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      val mapView = map.rangeImpl(from, until)
      mapView.iterator.toSeq == entriesInView(entries, from, until).toSeq.sorted
    }

    property("iteratorFrom") = forAll { (entries: Map[K, V], k: K, from: Option[K], until: Option[K]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      val mapView = map.rangeImpl(from, until)
      val newLower = Some(from.fold(k)(ord.max(_, k)))
      mapView.iteratorFrom(k).toSeq == entriesInView(entries, newLower, until).toSeq.sorted
    }

    property("keysIteratorFrom") = forAll { (entries: Map[K, V], k: K, from: Option[K], until: Option[K]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      val mapView = map.rangeImpl(from, until)
      val newLower = Some(from.fold(k)(ord.max(_, k)))
      mapView.keysIteratorFrom(k).toSeq == entriesInView(entries, newLower, until).toSeq.sorted.map(_._1)
    }

    property("valuesIteratorFrom") = forAll { (entries: Map[K, V], k: K, from: Option[K], until: Option[K]) =>
      val map = mutable.TreeMap[K, V]()
      map ++= entries

      val mapView = map.rangeImpl(from, until)
      val newLower = Some(from.fold(k)(ord.max(_, k)))
      mapView.valuesIteratorFrom(k).toSeq == entriesInView(entries, newLower, until).toSeq.sorted.map(_._2)
    }

    property("headOption") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
      val mapView = map.rangeImpl(from, until)
      mapView.headOption == Try(entriesInView(map.iterator, from, until).next()).toOption
    }

    property("lastOption") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
      val mapView = map.rangeImpl(from, until)
      mapView.lastOption == Try(entriesInView(map.iterator, from, until).max).toOption
    }

    property("clear") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
      val mapView = map.rangeImpl(from, until)
      mapView.clear()
      map.isEmpty && mapView.isEmpty && map.size == 0 && mapView.size == 0
    }

    property("serializable") = forAll { (map: mutable.TreeMap[K, V], from: Option[K], until: Option[K]) =>
      val mapView = map.rangeImpl(from, until)

      val bytesOut = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytesOut)
      out.writeObject(mapView)
      val bytes = bytesOut.toByteArray

      val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
      val sameMapView = in.readObject().asInstanceOf[mutable.TreeMap[K, V]]
      mapView.iterator.toSeq == sameMapView.iterator.toSeq
    }
  }
}

object Test extends Properties("mutable.TreeMap") {
  import scala.collection.mutable._
  include(RedBlackTreeProperties)
  include(MutableTreeMapProperties)
  include(MutableTreeMapViewProperties)
}
