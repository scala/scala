package scala.reflect.internal.names

import java.util.concurrent.ConcurrentHashMap

class ConcurrentMapNameTable1[T <: AnyRef](builder: (String) => T) extends NameTable[T] {

  override def size: Int = data.size

  private val data = new ConcurrentHashMap[String, T]

  def find(source: String): T = data.computeIfAbsent(source, (s) => builder(s))

  override def nonAllocatingStringLookup: Boolean = false
}
class ConcurrentMapNameTable2[T <: AnyRef](builder: (String) => T) extends NameTable[T] {

  override def size: Int = data.size

  private val data = new ConcurrentHashMap[String, T]
  private val builderFn = new java.util.function.Function[String, T]{
    override def apply(t: String): T = builder(t)
  }

  def find(source: String): T = data.computeIfAbsent(source, builderFn)
}
class ConcurrentMapNameTable3[T <: AnyRef](builder: (String) => T) extends NameTable[T] {

  override def size: Int = data.size

  private val data = new ConcurrentHashMap[String, T]

  def find(source: String): T = {
    var res = data.get(source)
    if (res eq null) {
      res = builder(source)
      val existing = data.putIfAbsent(source, res)
      if (existing ne null)
        res = existing
    }
    res
  }
}
