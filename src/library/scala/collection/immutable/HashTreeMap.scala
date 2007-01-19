/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$



package scala.collection.immutable


object HashTreeMap {
  /** The empty map of this type */
  def empty[A, B] = new HashTreeMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: Pair[A, B]*) = empty[A, B] ++ elems
}

/** An immutable tree-based map that uses hashes to order
 * the elements.  While it is not as efficient as
 * a regular TreeMap, it has the advantage of not requiring
 * an Ordered view being available for the key data.
 *
 * The implementation a TreeMap mapping hash codes into
 * ListMap's, where each ListMap is a bucket of entries
 * whose keys have the same hash code.
 */
class HashTreeMap[Key, +Value](treemap: TreeMap[int, Map[Key, Value]],  val size: int)
extends Map[Key, Value]
{
  /** Create an empty HashTreeMap */
  def this() = this(new TreeMap, 0)

  /** A central, convenenient updating function.  Given a key,
    * it finds the bucket for that key, allows the specified
    * routine to update the bucket, and then creates a new
    * HashTreeMap using the updated bucket.
    */
  private def updateBucket[Value1 >: Value]
    (key: Key)
    (upd: Map[Key, Value1] => Map[Key, Value1])
    : Map[Key, Value1] =
  {
    val hash: int = key.hashCode
    var sizechange = 0

    val newmap =
      treemap.updatef(
          hash,
          maybeBucket => {
            val oldBucket = maybeBucket match {
              case None => new ListMap[Key, Value]
              case Some(oldBucket) => oldBucket
            }
            val newBucket = upd(oldBucket)
            sizechange = newBucket.size - oldBucket.size
            newBucket
          })

    new HashTreeMap(newmap, size + sizechange)
  }

  def update[Value1 >: Value](key: Key, value: Value1) =
    updateBucket[Value1](key)(b => b + {key, value})

  def get(key: Key) = {
    treemap.get(key.hashCode) match {
      case None => None
      case Some(bucket) => bucket.get(key)
    }
  }

  def - (key: Key) = updateBucket(key)(b => b - key)

  def empty[C] = new HashTreeMap[Key, C]

  def elements = new scala.Iterator[Pair[Key,Value]] {
    val topElements = treemap.elements
    var bucket: Iterator[Pair[Key, Value]] = null

    def moveToNext = {
      while(topElements.hasNext &&
            (bucket == null || !bucket.hasNext))
        bucket = topElements.next._2.elements
    }

    def hasNext = {
      moveToNext
      (bucket != null) && bucket.hasNext
    }

    def next = {
      moveToNext
      bucket.next
    }
  }
}