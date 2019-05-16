package scala.reflect.internal.names

import java.util.concurrent.atomic.AtomicReferenceArray

import scala.annotation.tailrec

object StrongConcurrentNodeInterner {
  private val Flag: StrongNode[_] = new StrongNode(null, null)

  @inline private def FlagN[N >: Null <: NameBase]: StrongNode[N] = Flag.asInstanceOf[StrongNode[N]]
}

/**
  * a Node Interner that allows for concurrent access
  * references to Names are held strongly
  *
  * Data table can only even grow
  *
  * Data table head is only appended with CAS
  * A data table row may be modified by CAS to Flag, as no other thread can add to head when it eq Flag
  *
  * @param createName
  * @tparam T
  */
class StrongConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractConcurrentNodeInterner[T](createName) {
  type N = StrongNode[T]

  import StrongConcurrentNodeInterner._

  final def find(key: String): T = {
    val hash = key.hashCode
    val improved = improveHash(hash)
    var oldTail: N = null

    var name: T = null
    do {
      val data = initial()
      val bucket = improved & (data.length - 1)
      val head = data.get(bucket)
      if (head ne Flag) {
        if (head ne null)
          name = StrongNode.find(head, key, hash, oldTail)
        if (name eq null) {
          // minor optimisation - we can skip this tail if we have to retry
          // if we came to the end of the chain of nodes we dont need to search the same tail if we fail and try again
          oldTail = head
          name = createName(key)
          val newNode = new N(name, head)
          if (data.compareAndSet(bucket, head, newNode)) {
            afterInsert(data)
          } else name = null
        }
      }
    } while (name eq null)
    name
  }

  override protected def find(head: StrongNode[T], key: String, hash: Int): T = {
    StrongNode.find(head, key, hash, null)
  }

  /**
    * rehash and grow
    */
  private def afterInsert(data: AtomicReferenceArray[N]): Unit = {
    val newSize = size_.incrementAndGet()
    val origLength = data.length
    if (origLength < newSize && origLength < (1 << 31)) {
      size_.synchronized {
        if (isCurrentData(data)) grow(data)
      }
    }
  }

  @tailrec private def grow(data: AtomicReferenceArray[N]): Unit = {
    val length = data.length

    if (length < size && length < (1 << 31)) {

      //we allocate this before nulling data as the rest is non heap allocating, and it eliminates OOM as a cause of failure
      val newData = new AtomicReferenceArray[N](length << 1)
      val mask = length

      var head0: N = null
      var head1: N = null
      var sourceIdx = 0
      // if the value has changed already then its not our problem
      setDataNull()

      while (sourceIdx < length) {
        head0 = null
        head1 = null
        var tail0: N = null
        var tail1: N = null
        var sourceNode = data.getAndSet(sourceIdx, FlagN)
        while (sourceNode ne null) {
          val hash = sourceNode.hash
          val improved = improveHash(hash)
          if ((improved & mask) == 0) {
            if (head0 eq null) head0 = sourceNode
            else tail0.next = sourceNode
            tail0 = sourceNode
          } else {
            if (head1 eq null) head1 = sourceNode
            else tail1.next = sourceNode
            tail1 = sourceNode
          }
          sourceNode = sourceNode.next
        }
        if (tail0 ne null) tail0.next = null
        if (tail1 ne null) tail1.next = null
        newData.set(sourceIdx, head0)
        newData.set(sourceIdx + mask, head1)
        sourceIdx += 1
      }
      setData(newData)
      grow(newData)
    }
  }
}
