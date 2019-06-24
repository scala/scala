package scala.reflect.internal.names

import scala.collection.mutable

class MapNameTable[T <: AnyRef](builder: (String) => T) extends NameTable[T] {
  override def size: Int = data.size
  private val data = new mutable.AnyRefMap[String, T]

  def find(source: String): T = data.getOrElseUpdate(source, builder(source))

  override def nonAllocatingStringLookup: Boolean = false
}
