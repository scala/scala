package scala.reflect.internal.names

import java.lang.ref.{ReferenceQueue, WeakReference}
import java.util.Map
import java.util.concurrent.ConcurrentHashMap
import java.util.function.Consumer

import scala.annotation.tailrec

abstract class AbstractWeakConcurrentMapNameTable1[T >: Null <: AnyRef](builder: (String) => T) extends NameTable[T] {

  type REF <: WeakReference[T]
  def newNameRef(key:String, name: T):REF

  protected val data = new ConcurrentHashMap[String, REF]

  override def size: Int = data.size

  def find(source: String): T = {
    var res: T = null
    //we assume that we usually find a match, so no lock on the fast path
    var entry = data.get(source)

    var maybe: T = null
    do {
      if (entry eq null) {
        if (maybe eq null) maybe = builder(source)
        entry = data.putIfAbsent(source, newNameRef(source, maybe))
        if (entry eq null)
          res = maybe
      } else {
        res = entry.get()
        if (res eq null) {
          if (maybe eq null) maybe = builder(source)
          if (data.replace(source, entry, newNameRef(source, maybe)))
            res = maybe
          else {
            res = null
            entry = data.get(source)
          }
        }
      }
    } while (res eq null)
    res
  }
}
abstract class AbstractWeakConcurrentMapNameTable2[T >: Null <: AnyRef](builder: (String) => T) extends NameTable[T] {

  type REF <: WeakReference[T]
  def newNameRef(key:String, name: T):REF

  protected val data = new ConcurrentHashMap[String, REF]

  override def size: Int = data.size

  private val remapper = new  java.util.function.BiFunction[String, REF, REF] {
    override def apply(key: String, existing: REF): REF =
      if ((existing eq null) || (existing.get eq null))
        newNameRef(key, builder(key))
      else existing
  }
  def find(source: String): T = {
    var res: T = null
    do res = data.compute(source, remapper).get
    while ( res eq null)
    res
  }
}
final class WeakNoAutoTrimConcurrentMapNameTable1[T >: Null <: AnyRef](builder: (String) => T) extends AbstractWeakConcurrentMapNameTable1(builder) {
  type REF = WeakReference[T]
  override def newNameRef(key:String, name: T):REF  = new WeakReference(name)

  private val removeCollected = new Consumer[Map.Entry[String, REF]] {
    override def accept(t: Map.Entry[String, REF]): Unit =
      if (t.getValue.get() == null) data.remove(t.getKey, t.getValue)
  }

  def trim() = data.forEachEntry(1, removeCollected)
}
final class WeakNoAutoTrimConcurrentMapNameTable2[T >: Null <: AnyRef](builder: (String) => T) extends AbstractWeakConcurrentMapNameTable2(builder) {
  type REF = WeakReference[T]
  override def newNameRef(key:String, name: T):REF  = new WeakReference(name)

  private val removeCollected = new Consumer[Map.Entry[String, REF]] {
    override def accept(t: Map.Entry[String, REF]): Unit =
      if (t.getValue.get() == null) data.remove(t.getKey, t.getValue)
  }

  def trim() = data.forEachEntry(1, removeCollected)
}
final class WeakAutoTrimConcurrentMapNameTable1[T >: Null <: AnyRef](builder: (String) => T) extends AbstractWeakConcurrentMapNameTable1(builder) {

  private val queue = new ReferenceQueue[T]
  type REF = NameRef
  class NameRef(val key: String, name: T) extends WeakReference(name, queue)

  override def newNameRef(key: String, name: T) = new NameRef(key, name)

  @tailrec def processQueue(): Unit = {
    val next = queue.poll().asInstanceOf[NameRef]
    if (next ne null) {
      data.get(next.key) match {
        case null =>
        case ref =>
          if (ref.get() eq null)
            data.compute(ref.key, (_: String, existing: NameRef) =>
              if (existing.get eq null) null
              else existing)
      }
      processQueue()
    }
  }

  override def size: Int = {
    processQueue()
    super.size
  }

  override def find(source: String): T = {
    processQueue()
    super.find(source)
  }
}
final class WeakAutoTrimConcurrentMapNameTable2[T >: Null <: AnyRef](builder: (String) => T) extends AbstractWeakConcurrentMapNameTable2(builder) {

  private val queue = new ReferenceQueue[T]
  type REF = NameRef
  class NameRef(val key: String, name: T) extends WeakReference(name, queue)

  override def newNameRef(key: String, name: T) = new NameRef(key, name)

  @tailrec def processQueue(): Unit = {
    val next = queue.poll().asInstanceOf[NameRef]
    if (next ne null) {
      data.get(next.key) match {
        case null =>
        case ref =>
          if (ref.get() eq null)
            data.compute(ref.key, (_: String, existing: NameRef) =>
              if (existing.get eq null) null
              else existing)
      }
      processQueue()
    }
  }

  override def size: Int = {
    processQueue()
    super.size
  }

  override def find(source: String): T = {
    processQueue()
    super.find(source)
  }
}
