package scala.reflect.internal.names

import java.lang.ref.WeakReference
import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceFieldUpdater}

import scala.annotation.tailrec

abstract class Node[N >: Null <: NameBase] extends AnyRef {
  def name: N

  def namesString: String

  def hash: Int
}

object StrongNode {
  final def find[N >: Null <: NameBase](start: StrongNode[N], key: String, hash: Int, end: StrongNode[N]): N = {
    var current = start
    while ((current ne null) && (start ne end)) {
      val nameString = current.namesString
      if (nameString.hashCode == hash && nameString == key) return current.name
      current = current.next
    }
    null
  }

}
final class StrongNode[N >: Null <: NameBase](val name: N, var next: StrongNode[N]) extends Node[N] {
  @inline def hash: Int = namesString.hashCode()

  @inline def namesString: String = name.id


}

object WeakNode {

  @inline final def find[N >: Null <: NameBase](start: WeakNode[N], key: String, hash: Int, decrementOnRemove: AtomicInteger): N = {
    var current = start
    var currentName = current.name
    if ((currentName ne null) && currentName.id.hashCode() == hash && currentName.id == key)
      currentName
    else {
      var prev: WeakNode[N] = current
      var found: N = null
      current = current.next
      while ((found eq null) && (current ne null)) {
        currentName = current.name
        if (currentName eq null) {
          val next = current.next
          prev.next = next
          current = next
          decrementOnRemove.decrementAndGet()
        } else if (currentName.id.hashCode() == hash && currentName.id == key)
          found = currentName
        else {
          prev = current
          current = current.next
        }
      }
      found
    }
  }
}

final class WeakNode[N >: Null <: NameBase](k: N, @volatile var next: WeakNode[N]) extends Node[N] {
  private val ref = new WeakReference(k)

  def namesString: String = {
    val n = name
    if (n eq null) null else n.id
  }

  def name = ref.get()

  def isDefined = ref.get() ne null

  def hash: Int = {
    val n = name
    if (n eq null) 0 else n.hashCode
  }

}

object WeakConcurrentNode {
  private val nextAccess: AtomicReferenceFieldUpdater[WeakConcurrentNode[_], WeakConcurrentNode[_]] = new WeakConcurrentNode[NameBase](null, null).nextAccess

  private def nextAccessN[N >: Null <: NameBase]: AtomicReferenceFieldUpdater[WeakConcurrentNode[N], WeakConcurrentNode[N]] = nextAccess.asInstanceOf[AtomicReferenceFieldUpdater[WeakConcurrentNode[N], WeakConcurrentNode[N]]]

  private def casNext[N >: Null <: NameBase](prev: WeakConcurrentNode[N], expectedNext: WeakConcurrentNode[N], updateNext: WeakConcurrentNode[N]): Boolean = {
    nextAccess.compareAndSet(prev, expectedNext, updateNext)
  }

  private val builderCache = new ThreadLocal[java.lang.StringBuilder] {
    override def initialValue() = new java.lang.StringBuilder
  }

  final def findNoTrimC1[N >: Null <: NameBase](start: WeakConcurrentNode[N], chars: Array[Char], charOffset: Int, charCount: Int, hash: Int): N = {
    def equalsAsString(nameId: String): Boolean = {
      nameId.hashCode() == hash && nameId.length == charCount && {
        var offset = 0
        var equal = true
        while (offset < charCount && equal) {
          equal = chars(charOffset + offset) == nameId.charAt(offset)
          offset += 1
        }
        equal
      }
    }

    var current = start
    var prev: WeakConcurrentNode[N] = null

    var currentName: N = null
    var found: N = null

    do {
      currentName = current.name
      if ((currentName ne null) && equalsAsString(currentName.id))
        found = currentName
      else {
        prev = current
        current = current.next
      }
    } while ((found eq null) && (current ne null))
    found
  }

  final def findNoTrimC2[N >: Null <: NameBase](start: WeakConcurrentNode[N], chars: Array[Char], charOffset: Int, charCount: Int, hash: Int): N = {
    var seq: java.lang.StringBuilder = null
    var current = start
    var prev: WeakConcurrentNode[N] = null

    var currentName: N = null
    var found: N = null

    do {
      currentName = current.name
      if ((currentName ne null) && currentName.id.hashCode() == hash && currentName.id.length == charCount && {
        if (seq == null) {
          seq = new java.lang.StringBuilder()
          seq.append(chars, charOffset, charCount)
        }
        currentName.id.contentEquals(seq)
      })
        found = currentName
      else {
        prev = current
        current = current.next
      }
    } while ((found eq null) && (current ne null))
    found
  }

  final def findNoTrimC3[N >: Null <: NameBase](start: WeakConcurrentNode[N], chars: Array[Char], charOffset: Int, charCount: Int, hash: Int): N = {
    var seq: java.lang.StringBuilder = null
    var current = start
    var prev: WeakConcurrentNode[N] = null

    var currentName: N = null
    var found: N = null

    do {
      currentName = current.name
      if ((currentName ne null) && currentName.id.hashCode() == hash && currentName.id.length == charCount && {
        if (seq == null) {
          seq = builderCache.get()
          seq.append(chars, charOffset, charCount)
        }
        currentName.id.contentEquals(seq)
      })
        found = currentName
      else {
        prev = current
        current = current.next
      }
    } while ((found eq null) && (current ne null))
    if (seq ne null)
      seq.setLength(0)
    found
  }

  final def findNoTrim[N >: Null <: NameBase](start: WeakConcurrentNode[N], key: String, hash: Int, end: Node[N]): N = {
    var current = start
    var name: N = null

    do {
      name = current.name
      if ((name eq null) || name.id.hashCode() != hash || name.id != key) {
        name = null
        current = current.next
      }
    } while ((name eq null) && (current ne null) && (current ne end))
    name
  }

  final def findNoTrim[N >: Null <: NameBase](start: WeakConcurrentNode[N], key: String, hash: Int): N = {
    var current = start
    var name: N = null

    do {
      name = current.name
      if ((name eq null) || name.id.hashCode() != hash || name.id != key) {
        name = null
        current = current.next
      }
    } while ((name eq null) && (current ne null))
    name
  }

  /** trim the collected nodes
    * this is safe WRT other threads adding nodes ( as this occurs at the head
    * It is not threadsafe WRT other thread trimming
    *
    * @return the count of collected nodes
    */
  def trimCollected[N >: Null <: NameBase](start: WeakConcurrentNode[N]): Int = {
    var trimmed = 0
    var prev: WeakConcurrentNode[N] = start
    while (prev ne null) {
      val next = prev.next
      val nextValue = if (next eq null) null else next.name
      if ((next ne null) && (nextValue eq null)) {
        //Note - using a cas here doesnt make if safe WRT other trimmers as they may overlap in the list
        prev.next = next.next
        trimmed += 1
      } else {
        prev = next
      }
    }
    trimmed
  }

  //  final def findAutoTrim[N >: Null <: Name](start: WeakConcurrentNode[N], key: String, hash: Int, end: Node[N], decrementOnRemove: AtomicInteger): N = {
  //    if (start eq end)
  //      null
  //    else {
  //      //prev refers to the previos entry with a name that has not been GCed
  //      var prevRef: WeakConcurrentNode[N] = null
  //      var prevRefName: N = null
  //
  //      var current = start
  //      var currentName: N = null
  //      var found: N = null
  //
  //      var skipped = 0
  //
  //      do {
  //        currentName = current.name
  //        while ((current ne null) && (currentName eq null)) {
  //            val next = current.next
  //          skipped += 1
  //          }
  //        }
  //          //we dont care if we lost the race - someone else fixed it
  //          if ((prev ne null) && casNext(prev, current, next))
  //            decrementOnRemove.decrementAndGet()
  //          //leave prev where it was
  //          current = next
  //        } else if (currentName.id.hashCode() == hash && currentName.id == key)
  //          found = currentName
  //        else {
  //          prev = current
  //          current = current.next
  //        }
  //      } while ((found eq null) && (current ne null) && (current ne end))
  //      found
  //    }
  //  }
}

final class WeakConcurrentNode[N >: Null <: NameBase](private val ref: WeakReference[N], @volatile var next: WeakConcurrentNode[N]) extends Node[N] {
  private def nextAccess = AtomicReferenceFieldUpdater.newUpdater(classOf[WeakConcurrentNode[_]], classOf[WeakConcurrentNode[_]], "next")

  def getAndClearNext(): WeakConcurrentNode[N] = WeakConcurrentNode.nextAccessN[N].getAndSet(this, null)

  def namesString: String = {
    val n = name
    if (n eq null) null else n.id
  }

  def name = ref.get()

  def isDefined = ref.get() ne null

  def hash: Int = {
    val n = name
    if (n eq null) 0 else n.hashCode
  }

}
