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
import scala.collection.{MapFactory, MapFactoryDefaults}
import scala.collection.immutable.{LinkedHashMap => LHM}
final class LinkedHashMap[K, +V] private (private val _first: LHM.Link[K, V], private val _last: LHM.Link[K, V], hm: HashMap[K, LHM.Link[K, V]])
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with MapFactoryDefaults[K, V, LinkedHashMap, Iterable] {

  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap

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

  override def removed(key: K): LinkedHashMap[K, V] = ???
  override def updated[V1 >: V](key: K, value: V1): LinkedHashMap[K, V1] = {
    hm.get(key) match {
      case Some(old) =>
        if (old.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
          this
        } else {
          val oldPrevLinks = allPreviousLinksInChain(old, includeArgument = false)
          val newPrevLinks = oldPrevLinks.foldLeft(List.empty[(K, LHM.Link[K, V])]){
            case (acc @ h :: _, link) =>
              val copiedLink = link.copy()
              h._2.next = copiedLink
              (link.key, copiedLink) :: acc
            case (Nil, link) =>
              (link.key, link.copy()) :: Nil
          }

          val newLink = new LHM.Link(key, value, old.prev, old.next)

          newPrevLinks match {
            case h :: _ => h._2.next = newLink
            case _ =>
          }

          val allNewLinks = (newLink.key, newLink) :: newPrevLinks

          val newHm = hm.concat(allNewLinks)

          var newFirst: LHM.Link[K, V1] = _first
          var newLast: LHM.Link[K, V1] = _last

          if (newFirst == old) {
            newFirst = newLink
          } else if (newFirst == null) {
            newFirst = newLink
            newLast = newLink
          } else if(oldPrevLinks.nonEmpty && (oldPrevLinks.head eq _first)) {
            newFirst = newPrevLinks.last._2
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
        val newPrevLinks = oldPrevLinks.foldLeft(List.empty[(K, LHM.Link[K, V])]){
          case (acc @ h :: _, link) =>
            val copiedLink = link.copy()
            h._2.next = copiedLink
            (link.key, copiedLink) :: acc
          case (Nil, link) =>
            (link.key, link.copy()) :: Nil
        }

        val newPrevLink = newPrevLinks.head._2
        val newLink = new LHM.Link(key, value, newPrevLink.key, LHM.End)
        newPrevLink.next = if (hm.size % LHM.arity == 0) key else newLink

        val newHm = hm.concat((key, newLink) :: newPrevLinks)

        var newFirst: LHM.Link[K, V1] = _first
        if (oldPrevLinks.head eq _first) {
          newFirst = newPrevLinks.last._2
        }

        new LinkedHashMap(newFirst, newLink, newHm)
    }
  }
  override def get(key: K): Option[V] = hm.getOrElse(key, LHM.End) match {
    case LHM.End => None
    case v => Some(v).asInstanceOf[Option[V]]
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
}

object LinkedHashMap extends MapFactory[LinkedHashMap] {
  private final def arity: Int = 3
  private final val End: AnyRef = new AnyRef {}
  private final class Link[K, +V](val key: K, var value: V @uncheckedVariance, val prev: Any, var next: Any) {
    def copy(): Link[K, V] = new Link(key, value, prev, next)
    def tuple: (K, V) = (key, value)
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
        super.addAll(xs)
      }
    }
}
