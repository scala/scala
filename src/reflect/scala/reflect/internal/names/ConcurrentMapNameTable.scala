package scala.reflect.internal.names

import java.util.concurrent.ConcurrentHashMap

class ConcurrentMapNameTable[T <: AnyRef](builder: (String) => T) extends NameTable[T] {

  override def size: Int = data.size

  private val data = new ConcurrentHashMap[String, T]

  def find(source: String): T = data.computeIfAbsent(source, (s) => builder(s))
}
