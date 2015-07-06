import java.io._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Try

package scala.collection.mutable {

  object MutableTreeSetProperties extends Properties("mutable.TreeSet") {
    type K = String

    property("size, isEmpty") = forAll { (elems: Set[K]) =>
      val set = mutable.TreeSet[K]()
      set ++= elems
      set.size == elems.size && set.isEmpty == elems.isEmpty
    }

    property("+=") = forAll { (set: mutable.TreeSet[K], k: K) =>
      val oldSize = set.size
      val containedKeyBefore = set.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize else oldSize + 1

      set += k
      set.contains(k) && set.size == newExpectedSize
    }

    property("++=") = forAll { (set: mutable.TreeSet[K], ks: Seq[K]) =>
      val oldElems = set.toList
      set ++= ks
      (oldElems ++ ks).forall(set.contains)
    }

    property("-=") = forAll { (set: mutable.TreeSet[K], k: K) =>
      val oldSize = set.size
      val containedKeyBefore = set.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize - 1 else oldSize

      set -= k
      !set.contains(k) && set.size == newExpectedSize
    }

    property("--=") = forAll { (set: mutable.TreeSet[K], ks: Seq[K]) =>
      val oldElems = set.toList
      set --= ks
      val deletedElems = ks.toSet
      oldElems.forall { e => set.contains(e) == !deletedElems(e) }
    }

    property("iterator") = forAll { (ks: Set[K]) =>
      val set = mutable.TreeSet[K]()
      set ++= ks

      set.iterator.toSeq == ks.toSeq.sorted
    }

    property("iteratorFrom, keysIteratorFrom") = forAll { (ks: Set[K], k: K) =>
      val set = mutable.TreeSet[K]()
      set ++= ks

      set.iteratorFrom(k).toSeq == ks.filter(_ >= k).toSeq.sorted
      set.keysIteratorFrom(k).toSeq == ks.filter(_ >= k).toSeq.sorted
    }

    property("headOption") = forAll { (set: mutable.TreeSet[K]) =>
      set.headOption == Try(set.iterator.next()).toOption
    }

    property("lastOption") = forAll { (set: mutable.TreeSet[K]) =>
      set.lastOption == Try(set.iterator.max).toOption
    }

    property("clear") = forAll { (set: mutable.TreeSet[K]) =>
      set.clear()
      set.isEmpty && set.size == 0
    }

    property("serializable") = forAll { (set: mutable.TreeSet[K]) =>
      val bytesOut = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytesOut)
      out.writeObject(set)
      val bytes = bytesOut.toByteArray

      val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
      val sameSet = in.readObject().asInstanceOf[mutable.TreeSet[K]]
      set.iterator.toSeq == sameSet.iterator.toSeq
    }

    property("same behavior as immutable.TreeMap") = forAll { ops: Seq[Either[K, K]] =>
      var iset = immutable.TreeSet[K]()
      val mset = mutable.TreeSet[K]()

      ops.foreach {
        case Left(k) => iset += k; mset += k
        case Right(k) => iset -= k; mset -= k
      }

      iset.toList == mset.toList
    }
  }

  object MutableTreeSetViewProperties extends Properties("mutable.TreeSetView") {
    type K = String

    implicit val ord = implicitly[Ordering[K]]

    def in(key: K, from: Option[K], until: Option[K]) =
      from.fold(true)(_ <= key) && until.fold(true)(_ > key)

    def keysInView[This <: TraversableOnce[K], That](keys: This, from: Option[K], until: Option[K])(implicit bf: CanBuildFrom[This, K, That]) = {
      (bf.apply(keys) ++= keys.filter(in(_, from, until))).result()
    }

    property("size, isEmpty") = forAll { (keys: Set[K], from: Option[K], until: Option[K]) =>
      val map = mutable.TreeSet[K]()
      map ++= keys

      val mapView = map.rangeImpl(from, until)
      mapView.size == keysInView(keys, from, until).size &&
        mapView.isEmpty == !keys.exists(in(_, from, until))
    }

    property("+=") = forAll { (set: mutable.TreeSet[K], k: K, from: Option[K], until: Option[K]) =>
      val oldSize = set.size
      val containedKeyBefore = set.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize else oldSize + 1
      val isInRange = in(k, from, until)

      val setView = set.rangeImpl(from, until)
      setView += k

      set.contains(k) && set.size == newExpectedSize && setView.contains(k) == isInRange
    }

    property("++=") = forAll { (set: mutable.TreeSet[K], ks: Seq[K], from: Option[K], until: Option[K]) =>
      val setView = set.rangeImpl(from, until)
      setView ++= ks
      ks.toSet.forall { k =>
        set.contains(k) && setView.contains(k) == in(k, from, until)
      }
    }

    property("-=") = forAll { (set: mutable.TreeSet[K], k: K, from: Option[K], until: Option[K]) =>
      val oldSize = set.size
      val containedKeyBefore = set.contains(k)
      val newExpectedSize = if(containedKeyBefore) oldSize - 1 else oldSize

      val setView = set.rangeImpl(from, until)
      setView -= k

      !set.contains(k) && set.size == newExpectedSize && !setView.contains(k)
    }

    property("--=") = forAll { (set: mutable.TreeSet[K], ks: Seq[K], from: Option[K], until: Option[K]) =>
      val setView = set.rangeImpl(from, until)
      setView --= ks
      ks.toSet.forall { k => !set.contains(k) && !setView.contains(k) }
    }

    property("iterator") = forAll { (ks: Set[K], from: Option[K], until: Option[K]) =>
      val set = mutable.TreeSet[K]()
      set ++= ks

      val setView = set.rangeImpl(from, until)
      setView.iterator.toSeq == keysInView(ks, from, until).toSeq.sorted
    }

    property("iteratorFrom, keysIteratorFrom") = forAll { (ks: Set[K], k: K, from: Option[K], until: Option[K]) =>
      val set = mutable.TreeSet[K]()
      set ++= ks

      val setView = set.rangeImpl(from, until)
      val newLower = Some(from.fold(k)(ord.max(_, k)))
      setView.iteratorFrom(k).toSeq == keysInView(ks, newLower, until).toSeq.sorted
    }

    property("headOption") = forAll { (set: mutable.TreeSet[K], from: Option[K], until: Option[K]) =>
      val setView = set.rangeImpl(from, until)
      setView.headOption == Try(keysInView(set.iterator, from, until).next()).toOption
    }

    property("lastOption") = forAll { (set: mutable.TreeSet[K], from: Option[K], until: Option[K]) =>
      val setView = set.rangeImpl(from, until)
      setView.lastOption == Try(keysInView(set.iterator, from, until).max).toOption
    }

    property("clear") = forAll { (set: mutable.TreeSet[K], from: Option[K], until: Option[K]) =>
      val setView = set.rangeImpl(from, until)
      setView.clear()
      set.isEmpty && setView.isEmpty && set.size == 0 && setView.size == 0
    }

    property("serializable") = forAll { (set: mutable.TreeSet[K], from: Option[K], until: Option[K]) =>
      val setView = set.rangeImpl(from, until)

      val bytesOut = new ByteArrayOutputStream()
      val out = new ObjectOutputStream(bytesOut)
      out.writeObject(setView)
      val bytes = bytesOut.toByteArray

      val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
      val sameSetView = in.readObject().asInstanceOf[mutable.TreeSet[K]]
      setView.iterator.toSeq == sameSetView.iterator.toSeq
    }
  }
}

object Test extends Properties("mutable.TreeSet") {
  import scala.collection.mutable._
  include(MutableTreeSetProperties)
  include(MutableTreeSetViewProperties)
}
