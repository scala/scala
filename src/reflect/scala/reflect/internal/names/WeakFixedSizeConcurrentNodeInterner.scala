package scala.reflect.internal.names

import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray, AtomicReferenceFieldUpdater}
import java.lang.ref._
import java.util

/**
  * a Node Interner that allows for concurrent access
  * references to Names are held weakly
  *
  * Data table has a fixed entry size
  *
  * Data table head is only appended with CAS
  * a data row is trimmed when traversed, with CAS
  *
  * @param createName
  * @tparam T
  */
sealed abstract class AbstractWeakFixedSizeConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractNodeInterner[T](createName) {
  type N = WeakConcurrentNode[T]

  def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int): N

  def find(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)
    var oldTail: N = null

    var name: T = null
    val bucket = improved & mask
    do {
      val head = data.get(bucket)
      if (head ne null)
        name = WeakConcurrentNode.findNoTrim(head, key, hash, oldTail)
      if (name eq null) {
        name = createName(key)
        val newNode = createNode(name, head, bucket)
        if (data.compareAndSet(bucket, head, newNode)) {
          size_.incrementAndGet()
        } else {
          name = null
          // minor optimisation - we can skip this tail if we have to retry
          // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
          oldTail = head
        }
      }
    } while (name eq null)
    name
  }
  def findHash(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)
    var oldTail: N = null

    var name: T = null
    val bucket = improved & mask
    do {
      val head = data.get(bucket)
      if (head ne null)
        name = WeakConcurrentNode.findNoTrimHash(head, key, hash, oldTail)
      if (name eq null) {
        name = createName(key)
        val newNode = createNode(name, head, bucket)
        if (data.compareAndSet(bucket, head, newNode)) {
          size_.incrementAndGet()
        } else {
          name = null
          // minor optimisation - we can skip this tail if we have to retry
          // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
          oldTail = head
        }
      }
    } while (name eq null)
    name
  }
  def findNoHash(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)
    var oldTail: N = null

    var name: T = null
    val bucket = improved & mask
    do {
      val head = data.get(bucket)
      if (head ne null)
        name = WeakConcurrentNode.findNoTrimNoHash(head, key, hash, oldTail)
      if (name eq null) {
        name = createName(key)
        val newNode = createNode(name, head, bucket)
        if (data.compareAndSet(bucket, head, newNode)) {
          size_.incrementAndGet()
        } else {
          name = null
          // minor optimisation - we can skip this tail if we have to retry
          // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
          oldTail = head
        }
      }
    } while (name eq null)
    name
  }
  def findNoTail(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)

    var name: T = null
    val bucket = improved & mask
    do {
      val head = data.get(bucket)
      if (head ne null)
        name = WeakConcurrentNode.findNoTrim(head, key, hash)
      if (name eq null) {
        name = createName(key)
        val newNode = createNode(name, head, bucket)
        if (data.compareAndSet(bucket, head, newNode)) {
          size_.incrementAndGet()
        } else name = null
      }
    } while (name eq null)
    name
  }

  //only public until we select an implementation
  def find1(chars: Array[Char], start: Int, count: Int): T = {
    val hash = hashLikeString(chars, start, count)
    val improved = improveHash(hash)

    var name: T = null
    val bucket = improved & mask
    val head = data.get(bucket)
    if (head ne null)
      name = WeakConcurrentNode.findNoTrimC1(head, chars, start, count, hash)
    if (name eq null) {
      val string = new String(chars, start, count)
      name = createName(string)
      val newNode = createNode(name, head, bucket)
      if (data.compareAndSet(bucket, head, newNode)) {
        size_.incrementAndGet()
      } else name = find(string)
    }
    name
  }

  //only public until we select an implementation
  def find2(chars: Array[Char], start: Int, count: Int): T = {
    val hash = hashLikeString(chars, start, count)
    val improved = improveHash(hash)

    var name: T = null
    val bucket = improved & mask
    val head = data.get(bucket)
    if (head ne null)
      name = WeakConcurrentNode.findNoTrimC2(head, chars, start, count, hash)
    if (name eq null) {
      val string = new String(chars, start, count)
      name = createName(string)
      val newNode = createNode(name, head, bucket)
      if (data.compareAndSet(bucket, head, newNode)) {
        size_.incrementAndGet()
      } else name = find(string)
    }
    name
  }

  //only public until we select an implementation
  def find3(chars: Array[Char], start: Int, count: Int): T = {
    val hash = hashLikeString(chars, start, count)
    val improved = improveHash(hash)

    var name: T = null
    val bucket = improved & mask
    val head = data.get(bucket)
    if (head ne null)
      name = WeakConcurrentNode.findNoTrimC3(head, chars, start, count, hash)
    if (name eq null) {
      val string = new String(chars, start, count)
      name = createName(string)
      val newNode = createNode(name, head, bucket)
      if (data.compareAndSet(bucket, head, newNode)) {
        size_.incrementAndGet()
      } else name = find(string)
    }
    name
  }

  private val size_ = new AtomicInteger()

  override def size: Int = size_.get

  override def getExistingImpl(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)

    val head = data.get(improved & (data.length - 1))
    if (head eq null) null
    else WeakConcurrentNode.findNoTrim(head, key, hash)

  }

  protected final def trimImpl(index: Int): Unit = {
    var head: N = null
    var value: AnyRef = null
    do {
      head = data.get(index)
      if (head eq null)
        value = this
      else {
        value = head.name
        if ((value eq null) && data.compareAndSet(index, head, head.next))
          size_.decrementAndGet()
      }
    } while (value eq null)

    size_.addAndGet(-WeakConcurrentNode.trimCollected(head))
  }

  @inline final val data = new AtomicReferenceArray[N](1 << 16)

  @inline final private def mask = (1 << 16) - 1
}

class WeakFixedSizeNoAutoTrimConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractWeakFixedSizeConcurrentNodeInterner(createName) {
  override def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int) = new WeakConcurrentNode[T](new WeakReference(name), next)

  def trim() = synchronized {
    for (index <- 0 until data.length)
      trimImpl(index)
  }
}
class WeakFixedSizeNoAutoTrimConcurrentNodeInternerHash[T >: Null <: NameBase](createName: String => T) extends AbstractWeakFixedSizeConcurrentNodeInterner(createName) {
  override def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int) = new WeakConcurrentNode[T](new WeakReference(name), next)

  def trim() = synchronized {
    for (index <- 0 until data.length)
      trimImpl(index)
  }

  override def find(key: String): T = {
    super.findHash(key)
  }

}
class WeakFixedSizeNoAutoTrimConcurrentNodeInternerNoHash[T >: Null <: NameBase](createName: String => T) extends AbstractWeakFixedSizeConcurrentNodeInterner(createName) {
  override def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int) = new WeakConcurrentNode[T](new WeakReference(name), next)

  def trim() = synchronized {
    for (index <- 0 until data.length)
      trimImpl(index)
  }

  override def find(key: String): T = {
    super.findNoHash(key)
  }

}

class WeakFixedSizeAutoTrimConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractWeakFixedSizeConcurrentNodeInterner(createName) {
  private val queue = new ReferenceQueue[T]

  override def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int) = new WeakConcurrentNode[T](new BucketReference(name, queue, bucket), next)

  private class BucketReference(name: T, queue: ReferenceQueue[T], val bucket: Int) extends WeakReference[T](name, queue)

  override def size: Int = {
    trimCollected()
    super.size
  }

  override def find(key: String): T = {
    trimCollected()
    super.find(key)
  }

  @inline private def trimCollected(): Unit = {
    val next = queue.poll()
    if (next ne null) {
      reallyTrim(next)
    }
  }

  private def reallyTrim(first: Reference[_]): Unit = {
    var next = first
    val buckets = new util.BitSet(data.length)
    do {
      buckets.set(next.asInstanceOf[BucketReference].bucket)
      next = queue.poll()
    } while (next ne null)

    queue.synchronized {
      var index = buckets.nextSetBit(0)
      while (index != -1) {
        trimImpl(index)
        index = buckets.nextSetBit(index + 1)
      }
    }
  }
}

object AbstractUnsafeWeakFixedSizeConcurrentNodeInterner {

  private[this] val unsafe = {
    val f = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
    f.setAccessible(true)
    f.get(null).asInstanceOf[sun.misc.Unsafe]
  }
  private[this] val base = unsafe.arrayBaseOffset(classOf[Array[WeakConcurrentNode[_]]])
  private[this] val shift = {
    val scale = unsafe.arrayIndexScale(classOf[Array[WeakConcurrentNode[_]]])
    if (Integer.bitCount(scale) != 1)
      throw new Error("data type scale not a power of two")
    31 - Integer.numberOfLeadingZeros(scale)
  }

  @inline private[this] def offset(bucket: Int) =
    (bucket.toLong << shift) + base

  def compareAndSet[T >: Null <: NameBase](data: Array[WeakConcurrentNode[T]], bucket: Int, oldNode: WeakConcurrentNode[T], newNode: WeakConcurrentNode[T]): Boolean = {
    unsafe.compareAndSwapObject(data, offset(bucket), oldNode, newNode)
  }

  def getVolatile[T >: Null <: NameBase](data: Array[WeakConcurrentNode[T]], bucket: Int): WeakConcurrentNode[T] = {
    unsafe.getObjectVolatile(data, offset(bucket)).asInstanceOf[WeakConcurrentNode[T]]
  }
}

sealed abstract class AbstractUnsafeWeakFixedSizeConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractNodeInterner[T](createName) {

  import AbstractUnsafeWeakFixedSizeConcurrentNodeInterner._

  type N = WeakConcurrentNode[T]

  def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int): N

  def find(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)
    var oldTail: N = null

    var name: T = null
    val bucket = improved & mask
    do {
      val head = getVolatile(data, bucket)
      if (head ne null)
        name = WeakConcurrentNode.findNoTrim(head, key, hash, oldTail)
      if (name eq null) {
        name = createName(key)
        val newNode = createNode(name, head, bucket)
        if (compareAndSet(data, bucket, head, newNode)) {
          size_.incrementAndGet()
        } else {
          name = null
          // minor optimisation - we can skip this tail if we have to retry
          // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
          oldTail = head
        }
      }
    } while (name eq null)
    name
  }
  def findNoTail(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)

    var name: T = null
    val bucket = improved & mask
    do {
      val head = getVolatile(data, bucket)
      if (head ne null)
        name = WeakConcurrentNode.findNoTrim(head, key, hash)
      if (name eq null) {
        name = createName(key)
        val newNode = createNode(name, head, bucket)
        if (compareAndSet(data, bucket, head, newNode)) {
          size_.incrementAndGet()
        } else name = null
      }
    } while (name eq null)
    name
  }

  private val size_ = new AtomicInteger()

  override def size: Int = size_.get

  override def getExistingImpl(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)

    val head = getVolatile(data, improved & (data.length - 1))
    if (head eq null) null
    else WeakConcurrentNode.findNoTrim(head, key, hash)

  }

  protected final def trimImpl(index: Int): Unit = {
    var head: N = null
    var value: AnyRef = null
    do {
      head = getVolatile(data, index)
      if (head eq null)
        value = this
      else {
        value = head.name
        if ((value eq null) && compareAndSet(data, index, head, head.next))
          size_.decrementAndGet()
      }
    } while (value eq null)

    size_.addAndGet(-WeakConcurrentNode.trimCollected(head))
  }

  @inline final val data = new Array[N](1 << 16)

  @inline final private def mask = (1 << 16) - 1
}

class UnsafeWeakFixedSizeNoAutoTrimConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractUnsafeWeakFixedSizeConcurrentNodeInterner(createName) {
  override def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int) = new WeakConcurrentNode[T](new WeakReference(name), next)

  def trim() = synchronized {
    for (index <- 0 until data.length)
      trimImpl(index)
  }
}

class UnsafeWeakFixedSizeAutoTrimConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractUnsafeWeakFixedSizeConcurrentNodeInterner(createName) {
  private val queue = new ReferenceQueue[T]

  override def createNode(name: T, next: WeakConcurrentNode[T], bucket: Int) = new WeakConcurrentNode[T](new BucketReference(name, queue, bucket), next)

  private class BucketReference(name: T, queue: ReferenceQueue[T], val bucket: Int) extends WeakReference[T](name, queue)

  override def size: Int = {
    trimCollected()
    super.size
  }

  override def find(key: String): T = {
    trimCollected()
    super.find(key)
  }

  @inline private def trimCollected(): Unit = {
    val next = queue.poll()
    if (next ne null) {
      reallyTrim(next)
    }
  }

  private def reallyTrim(first: Reference[_]): Unit = {
    var next = first
    val buckets = new util.BitSet(data.length)
    do {
      buckets.set(next.asInstanceOf[BucketReference].bucket)
      next = queue.poll()
    } while (next ne null)

    queue.synchronized {
      var index = buckets.nextSetBit(0)
      while (index != -1) {
        trimImpl(index)
        index = buckets.nextSetBit(index + 1)
      }
    }
  }
}
