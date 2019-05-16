package scala.reflect.internal.names

import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray, AtomicReferenceFieldUpdater}

//object AbstractConcurrentNodeInterner {
//  private val dataAccess = AtomicReferenceFieldUpdater.newUpdater(classOf[AbstractConcurrentNodeInterner[_]], classOf[AtomicReferenceArray[_]], "data")
//}

abstract class AbstractConcurrentNodeInterner[T >: Null <: NameBase](createName: String => T) extends AbstractNodeInterner[T](createName) {

  //also serves as a synchronization point for a change to data
  protected[this] val size_ = new AtomicInteger
  def size = size_.get

  /**
    * all values in data are live. All values entered must be only entered after all checks to intern are completed
    * for child classes that resize a flag value is generally CASed into this array before a row is resized
    */
  @volatile private[this] var data = new AtomicReferenceArray[N](1 << 16)

  protected final def setDataNull(): Unit = {
    assert (Thread.holdsLock(size_))
    data = null
  }
  protected final def setData(newData: AtomicReferenceArray[N]): Unit = {
    assert(newData ne null)
    assert(data eq null)
    assert (Thread.holdsLock(size_))
    data = newData
  }
  protected final def isCurrentData(data: AtomicReferenceArray[N]): Boolean = {
    assert(data ne null)
    assert (Thread.holdsLock(size_))
    data eq this.data
  }


  def getExistingImpl(key: String): T = {
    val data = initial()
    val hash = key.hashCode()
    val improved = improveHash(hash)
    val head: N = data.get(improved & (data.length() - 1))
    if (head eq null) null
    else find(head,key, hash)
  }

  protected def find(start: N, key: String, hash:Int):T
  /**
    * get the root of data
    *
    * @return
    */
  protected def initial(): AtomicReferenceArray[N] = {
    //volatile read
    var result = data
    //null indicates it is in the process of being rehashed
    //updates are applied with synchronisation lock on data
    if (result eq null) size.synchronized {
      //when we have the lock we can guarantee that the other threads rehash is complete
      result = data
      assert(result ne null)
    }
    result
  }
}