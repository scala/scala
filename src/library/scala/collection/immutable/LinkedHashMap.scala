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

package scala.collection.immutable

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{Hashing, MapFactory, MapFactoryDefaults}
import scala.collection.immutable.{LinkedHashMap => LHM}


/** An immutable.SeqMap implementation similar to a mutable.LinkedHashMap, where key-values are stored in nodes(`Link`s),
 *  which refer forwards by REFERENCE to the next Link, and backwards by LOOKUP KEY to the previous Link.
 *
 * Referring forward by reference is beneficial for traversal, but to have acceptable update/remove performance, the
 * entire Map cannot be forward-linked by reference. That would necessetate a complete map reconstruction when performing
 * immutable writes. The forward-reference chain therefor breaks at regular intervals of `arity` nodes, where `arity`
 * is a configurable parameter in the companion object. After thorough benchmarking, it was found that `arity`= 2 strikes
 * the most sensible balance, as the costliness of updates becomes drastically greater for arities >=3, while benefit
 * to traversal diminishes.
 *
 * For instance, with `arity`= 2, we would have
 *
 * LinkedHashMap("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4, "e" -> 5", "f" -> 6, "g" -> 7)
 *
 * represented as:
 *
 *   * first                                                                                          * last
 *   |                                                                                                |
 *   |                                                                                                |
 *   Link("a",1) ===> Link("b",2) -> Link("c",3") ===> Link("d",4) -> Link("e",5) ===> Link("f",6) -> Link("g",7)
 *               <-               <-              <-               <-             <-               <-
 *
 *   legend: ===> is a reference by memory address
 *           ->   is a forward reference by KEY (via lookup in an immutable HashMap)
 *           <-   is a backward reference by KEY (via lookup in an immutable HashMap)
 *
 *
 * Performance Characteristics:
 *
 *   Construction: Extremely fast (fastest of all immutable SeqMaps)! O(n log n). Approximately as fast as constructing
 *   a HashMap, with minimal overhead involved in wrapping values in Link[K, V]` instances and mutating prev/next
 *   values during building.
 *
 *   Traversal/Iteration: Extremely fast (fastest of all immutable SeqMaps)! O(n + (1/arity) * n log n).
 *   Almost as fast as traversing a `List`, but every `arity` elements, a hashmap lookup is required. For reference,
 *   each lookup is approximately 25 ns.
 *
 *   Lookup: Extremely fast (on par with other immutable SeqMaps)! O(log n). Simply a lookup in a
 *   `HashMap[K, Link[K, V]]`, and then an additional dereference of the value. Virtually as fast as HashMap itself.
 *
 *   Updates: Poor, but not pathological (40% slower than VectorMap, faster than TreeSeqMap)! O((arity/2) * log n). When updating a key,
 *   all nodes that PRECEDE that node, in its `arity`-sized segment must be updated accordingly. Any nodes occurring after
 *   the node only refer backwards by key, not by reference, so they need not be adjusted. So an update is equivalent to
 *   roughly arity/2 lookups and arity/2 updates. The shallow mutation capabilities of immutable.HashMap are however
 *   utilized to mitigate the downside.
 *
 *   Removals: Poor, but not pathological (50% slower than VectorMap (probably optimizeable down to 40% slower))!
 *   O((arity) * log n). Virtually all the same characteristics as updates, except with removals, you must also replace
 *   the single Link that succeeds the removed Link, so that it refers back (by key) to the Link before the removed
 *   Link. Only the immediately succeeding node needs replacing though, not all succeeding links in the segment..
 *
 * @author Josh Lemer
 */
final class LinkedHashMap[K, +V] private (private val _first: LHM.Link[K, V], private val _last: LHM.Link[K, V], hm: HashMap[K, LHM.Link[K, V]])
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with MapFactoryDefaults[K, V, LinkedHashMap, Iterable] {

  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap

  /** Returns a List of all links that precede `link` in the hashmap, before there is a break in the forward references.
   *
   * The returned list will be forward-facing. That is, the first element will be the link in the segment that appears
   * earliest in the map. The next element immediately follows, etc.
   *
   * If `includeArgument` is true, then the argument will be included as the last element of the list. Otherwise, it
   * will not be included in the result.
   */
  private[this] def allPreviousLinksInChain(link: LHM.Link[K, V], includeArgument: Boolean): List[LHM.Link[K, V]] = {
    var result: List[LHM.Link[K, V]] = Nil
    if (includeArgument) result = link :: result
    var curr = link
    while(curr.prev.asInstanceOf[AnyRef] ne LHM.End) {
      val prevKey = curr.prev.asInstanceOf[K]
      val prevLink = hm(prevKey)
      if (prevLink.next.asInstanceOf[AnyRef] eq curr) {
        result = prevLink :: result
      } else {
        return result
      }
      curr = prevLink
    }
    result
  }

  /** Optimized method to concatenate a list of links onto `hm`, where the keys are the link's keys, and the values are
   * the links themselves. Uses the finicky/tricky shallow mutation capabilities of the immutable.HashMap structure.
   * */
  private[this] def concatToHm[V1 >: V](list: List[LHM.Link[K, V1]]): HashMap[K, LHM.Link[K, V1]] = {
    var rootNode: BitmapIndexedMapNode[K, LHM.Link[K, V1]] = hm.rootNode
    var curr = list
    while (!curr.isEmpty) {
      val next = curr.head
      val hash = next.key.##
      rootNode = rootNode.updated(next.key, next, hash, Hashing.improve(hash), 0, replaceValue = true)
      curr = curr.tail
      if (rootNode ne hm.rootNode) {
        var nodeMap = 0
        while (!curr.isEmpty) {
          val next = curr.head
          val hash = next.key.##
          nodeMap = rootNode.updateWithShallowMutations(next.key, next, hash, Hashing.improve(hash), 0, nodeMap)
          curr = curr.tail
        }
        return new HashMap(rootNode)
      }
    }
    hm
  }

  override def removed(key: K): LinkedHashMap[K, V] = {
    hm.getOrElse(key, null) match {
      case null => this
      case link =>
        if (hm.size == 1) {
          return empty
        } else if (link eq _first) {
          val newFirst = (link.next match {
            case l: LHM.Link[K, V] => l
            case k: K => hm(k)
          }).copy(prev = LHM.End)
          return new LinkedHashMap(newFirst, _last, hm.updated(newFirst.key, newFirst).removed(key))
        }

        val oldPrevLinks = allPreviousLinksInChain(hm(link.prev.asInstanceOf[K]), includeArgument = true)
        var newPrevLinks: List[LHM.Link[K, V]] = Nil

        {
          var curr = oldPrevLinks
          while (!curr.isEmpty) {
            val link = curr.head
            val copiedLink = link.copy()
            newPrevLinks match {
              case h :: _ =>
                h.next = copiedLink
              case _ =>
            }
            newPrevLinks = copiedLink :: newPrevLinks
            curr = curr.tail
          }
        }

        newPrevLinks match {
          case h :: _ => h.next = link.next
          case _ =>
        }

        val allNewLinks = link.next match {
          case oldNextLink: LHM.Link[K, V] =>
            oldNextLink.copy(prev = if (newPrevLinks.isEmpty) LHM.End else newPrevLinks.head.key) :: newPrevLinks
          case LHM.End =>
            newPrevLinks
          case k: K =>
            hm(k).copy(prev = if (newPrevLinks.isEmpty) LHM.End else newPrevLinks.head.key) :: newPrevLinks
        }

        val newHm = concatToHm(allNewLinks).removed(key)


        var newFirst: LHM.Link[K, V] = _first
        var newLast: LHM.Link[K, V] = _last

        if (_first eq link) {
          newFirst = allNewLinks.head
        } else if(_first eq oldPrevLinks.head) {
          newFirst = newPrevLinks.last
        }

        if ((_last eq link) || (_last.key.asInstanceOf[AnyRef] eq allNewLinks.head.key.asInstanceOf[AnyRef])) {
          newLast = allNewLinks.head
        }

        new LinkedHashMap(newFirst, newLast, newHm)
    }
  }
  override def updated[V1 >: V](key: K, value: V1): LinkedHashMap[K, V1] = {
//    if (LHM.arity == 1) return updatedArity1(key, value)
//    if (LHM.arity == 2) return updatedArity2(key, value)
    hm.get(key) match {
      case Some(old) =>
        if (old.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
          this
        } else {
          val oldPrevLinks = allPreviousLinksInChain(old, includeArgument = false)
          var newPrevLinks: List[LHM.Link[K, V]] = Nil

          {
            var curr = oldPrevLinks
            while (!curr.isEmpty) {
              val link = curr.head
              val copiedLink = link.copy()
              newPrevLinks match {
                case h :: _ =>
                  h.next = copiedLink
                case _ =>
              }
              newPrevLinks = copiedLink :: newPrevLinks
              curr = curr.tail
            }
          }

          val newLink = new LHM.Link(key, value, old.prev, old.next)

          newPrevLinks match {
            case h :: _ => h.next = newLink
            case _ =>
          }

          val allNewLinks = newLink :: newPrevLinks

          val newHm = concatToHm(allNewLinks)

          var newFirst: LHM.Link[K, V1] = _first
          var newLast: LHM.Link[K, V1] = _last

          if (newFirst == old) {
            newFirst = newLink
          } else if (newFirst == null) {
            newFirst = newLink
            newLast = newLink
          } else if(oldPrevLinks.nonEmpty && (oldPrevLinks.head eq _first)) {
            newFirst = newPrevLinks.last
          }

          if (newLast eq old) {
            newLast = newLink
          }
          new LinkedHashMap(newFirst, newLast, newHm)
        }
      case None =>
        if (_first == null) {
          val newLink = new LHM.Link(key, value, LHM.End, LHM.End)
          return new LinkedHashMap(newLink, newLink, HashMap.empty.updated(key, newLink))
        }

        val oldPrevLinks = allPreviousLinksInChain(_last, includeArgument = true)
        var newPrevLinks: List[LHM.Link[K, V]] = Nil

        {
          var curr = oldPrevLinks
          while (!curr.isEmpty) {
            val link = curr.head
            val copiedLink = link.copy()
            newPrevLinks match {
              case h :: _ =>
                h.next = copiedLink
              case _ =>
            }
            newPrevLinks = copiedLink :: newPrevLinks
            curr = curr.tail
          }
        }

        val newPrevLink = newPrevLinks.head
        val newLink = new LHM.Link(key, value, newPrevLink.key, LHM.End)
        newPrevLink.next = if (hm.size % LHM.arity == 0) key else newLink

        val newHm = concatToHm(newLink :: newPrevLinks)

        var newFirst: LHM.Link[K, V1] = _first
        if (oldPrevLinks.head eq _first) {
          newFirst = newPrevLinks.last
        }

        new LinkedHashMap(newFirst, newLink, newHm)
    }
  }
  /** implementation of updated, but hard-coded specialized for arity=1 */
  def updatedArity1[V1 >: V](key: K, value: V1): LinkedHashMap[K, V1] = {
    // assert(LHM.arity == 1)
    if (_last == null) {
      LHM.newSingleton(key, value)
    } else {
      hm.getOrElse(key, null) match {
        case null =>
          if (_first eq _last) {
            LHM.newDoubleton(_first.key, _first.value, key, value)
          } else {
            val newLink = new LHM.Link[K, V1](key, value, _last.key, LHM.End)
            val oldLastUpdated = _last.copy(next = key)
            new LinkedHashMap(_first, newLink, concatToHm(oldLastUpdated :: newLink :: Nil))
          }
        case old =>
          if (old.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
            this
          } else {
            val link = old.copy(value = value)

            new LinkedHashMap(if (_first eq old) link else _first, if (_last eq old) link else _last, hm.updated(key, link))
          }
      }
    }
  }
  /** implementation of updated, but hard-coded specialized for arity=2 */
  def updatedArity2[V1 >: V](key: K, value: V1): LinkedHashMap[K, V1] = {
    // assert(LHM.arity == 2)
    if (_last == null) {
      LHM.newSingleton(key, value)
    } else {
      hm.getOrElse(key, null) match {
        case null =>
          if (hm.size == 1) {
            LHM.newDoubletonArity2(_first.key, _first.value, key, value)
          } else {
            val newLink = new LHM.Link[K, V1](key, value, _last.key, LHM.End)
            val oldLastUpdated = _last.copy(next = if (hm.size % 2 == 0) key else newLink)
            var allNewNodes = oldLastUpdated :: newLink :: Nil
            var newFirst = _first
            if (_last.prev != LHM.End) {
              val oldLastPrev = hm(_last.prev.asInstanceOf[K])
              if (oldLastPrev.next.isInstanceOf[LHM.Link[K, V]]) {
                val newLastPrev = oldLastPrev.copy(next = oldLastUpdated)
                allNewNodes = newLastPrev :: allNewNodes
                if (oldLastPrev eq _first) {
                  newFirst = newLastPrev
                }
              }
            }
            new LinkedHashMap(newFirst, newLink, concatToHm(allNewNodes))
          }
        case old =>
          if (old.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
            this
          } else {
            val newLink = new LHM.Link[K, V1](key, value, old.prev, old.next)

            var newFirst: LHM.Link[K, V1] = _first
            var newLast: LHM.Link[K, V1] = _last

            var newPrevNode: LHM.Link[K, V1] = null

            newLink.prev match {
              case LHM.End =>
                newFirst = newLink
              case k =>
                val prev = hm(k.asInstanceOf[K])
                if (prev.next.isInstanceOf[LHM.Link[K, V]]) {
                  newPrevNode = prev.copy(next = newLink)
                  if (prev eq _first) {
                    newFirst = newPrevNode
                  }
                }
            }

            if (_last eq old) {
              newLast = newLink
            }

            new LinkedHashMap(newFirst, newLast, if (newPrevNode == null) hm.updated(key, newLink) else concatToHm(newPrevNode :: newLink :: Nil))

          }
      }
    }
  }

  override def get(key: K): Option[V] = hm.getOrElse(key, null) match {
    case null => None
    case v => Some(v.asInstanceOf[LHM.Link[K, V]].value).asInstanceOf[Option[V]]
  }

  override def iterator: Iterator[(K, V)] = {
    if (_first == null) {
      Iterator.empty
    } else new Iterator[(K, V)] {
      var curr: LHM.Link[K, V] = _first
      override def hasNext: Boolean = curr != null
      override def next(): (K, V) = {
        if (curr == null) {
          Iterator.empty.next()
        } else {
          val result = curr.tuple
          curr = curr.next match {
            case link: LHM.Link[K, V] => link
            case LHM.End => null
            case key => hm(key.asInstanceOf[K])
          }
          result
        }
      }
    }

  }

  override def foreach[U](f: ((K, V)) => U): Unit = {
    var curr = _first
    while (curr != null) {
      f(curr.tuple)
      curr = curr.next match { case LHM.End => null case l: LHM.Link[K, V] => l case k => hm(k.asInstanceOf[K])}
    }
  }

  override def foreachEntry[U](f: (K, V) => U): Unit = {
    var curr = _first
    while (curr != null) {
      f(curr.key, curr.value)
      curr = curr.next match { case LHM.End => null case l: LHM.Link[K, V] => l case k => hm(k.asInstanceOf[K])}
    }
  }
}

object LinkedHashMap extends MapFactory[LinkedHashMap] {
  final def arity: Int = 2
  private final val End: AnyRef = new AnyRef {}
  private final class Link[K, +V](val key: K, var value: V @uncheckedVariance, val prev: Any, var next: Any) {
    def copy[V1 >: V](key: K = key, value: V1 = value, prev: Any = prev, next: Any = next): Link[K, V1] = new Link(key, value, prev, next)
    def tuple: (K, V) = (key, value)
  }

  private def newSingleton[K, V](k: K, v: V): LinkedHashMap[K, V] = {
    val link = new Link(k, v, End, End)
    new LinkedHashMap(link, link, HashMap.empty.updated(k, link))
  }
  private def newDoubleton[K, V](k0: K, v0: V, k1: K, v1: V): LinkedHashMap[K, V] = {
    val link0 = new Link(k0, v0, End, k1)
    val link1 = new Link(k1, v1, k0, End)
    new LinkedHashMap(link0, link1, HashMap(k0 -> link0, k1 -> link1))
  }
  private def newDoubletonArity2[K, V](k0: K, v0: V, k1: K, v1: V): LinkedHashMap[K, V] = {
    val link1 = new Link(k1, v1, k0, End)
    val link0 = new Link(k0, v0, End, link1)
    new LinkedHashMap(link0, link1, HashMap(k0 -> link0, k1 -> link1))
  }

  private final val Empty: LinkedHashMap[Nothing, Nothing] = new LinkedHashMap[Nothing, Nothing](null, null, HashMap.empty)

  override def empty[K, V]: LinkedHashMap[K, V] = Empty.asInstanceOf[LinkedHashMap[K, V]]

  override def from[K, V](it: IterableOnce[(K, V)]): LinkedHashMap[K, V] =  newBuilder[K, V].addAll(it).result()

  override def newBuilder[K, V]: collection.mutable.Builder[(K, V), LinkedHashMap[K, V]] =
    new collection.mutable.Builder[(K, V), LinkedHashMap[K, V]] {
      private var size: Int = 0
      private var _first: Link[K, V] = null
      private var _last: Link[K, V] = null
      private var hm = new HashMapBuilder[K, Link[K, V]]

      override def clear(): Unit = {
        _first = null
        _last = null
        hm.clear()
        size = 0
      }

      /** Result collection consisting of all elements appended so far. */
      override def result(): LHM[K, V] = {
        val result = new LinkedHashMap[K, V](_first, _last, hm.result())
        clear()
        result
      }

      override def addOne(elem: (K, V)): this.type = {
        val old = hm.getOrElse(elem._1, null)
        if (old == null) {
          val link = new Link(elem._1, elem._2, if (_last == null) End else _last.key, End)
          if(_last != null) {
            if (size % arity != 0) {
              _last.next = link
            } else {
              _last.next = elem._1
            }
          }
          if (_first == null) {
            _first = link
          }
          hm.addOne(elem._1, link)
          size += 1
          _last = link
        } else {
          old.value = elem._2
        }
        this
      }

      override def addAll(xs: IterableOnce[(K, V)]): this.type = {
        var __first = _first
        var __last = _last
        var __size = size
        val iter = xs.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next()
          val old = hm.getOrElse(k, null)
          if (old == null) {
            val link = new Link(k, v, if (__last == null) End else __last.key, End)
            if(__last != null) {
              if (__size % arity != 0) {
                __last.next = link
              } else {
                __last.next = k
              }
            }
            if (__first == null) {
              __first = link
            }
            hm.addOne(k, link)
            __size += 1
            __last = link
          } else {
            old.value = v
          }
        }
        _first = __first
        _last = __last
        size = __size
        this
      }
    }
}
