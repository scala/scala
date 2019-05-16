package scala.reflect.internal.names

import java.lang.ref.{Reference, ReferenceQueue, WeakReference}

import scala.annotation.tailrec
import scala.collection.mutable

final class WeakMapNameTable[T <: AnyRef](builder: (String) => T) extends NameTable[T] {
  class NameRef(val key:String, name:T) extends WeakReference(name, queue)

  private val data = new mutable.AnyRefMap[String, NameRef]
  private val queue = new ReferenceQueue[T]


  @tailrec def processQueue(): Unit = {
    val next = queue.poll().asInstanceOf[NameRef]
    if (next ne null) {
      data.getOrNull(next.key) match {
        case null =>
        case ref =>
          if (ref.get() eq null)
            data.remove(ref.key)
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
    data.getOrNull(source) match {
      case null =>
        val res = builder(source)
        data(source) = new NameRef(source, res)
        res
      case ref =>
        var res = ref.get()
        if (res eq null) {
          res = builder(source)
          data(source) = new NameRef(source, res)
        }
        res
    }
  }
}
