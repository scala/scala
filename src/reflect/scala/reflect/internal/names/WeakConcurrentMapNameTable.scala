package scala.reflect.internal.names

import java.lang.ref.{ReferenceQueue, WeakReference}
import java.util.concurrent.ConcurrentHashMap

import scala.annotation.tailrec

final class WeakConcurrentMapNameTable[T >: Null <: AnyRef](builder: (String) => T) extends NameTable[T] {

  class NameRef(val key: String, name: T) extends WeakReference(name, queue)

  private def newNameRef(s: String) = new NameRef(s, builder(s))

  private val data = new ConcurrentHashMap[String, NameRef]
  private val queue = new ReferenceQueue[T]

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
    data.size
  }

  def find(source: String): T = {
    processQueue()
    var res: T = null
    //we assume that we usually find a match, so no lock on the fast path
    var entry = data.get(source)
    do {
      if (entry eq null) {
        val maybe = builder(source)
        entry = data.putIfAbsent(source, new NameRef(source, maybe))
        if (entry eq null)
          res = maybe
      } else {
        res = entry.get()
        if (res eq null) {
          val maybe = builder(source)
          if (data.replace(source, entry, new NameRef(source, maybe)))
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
