package scala.tools.nsc.backend.jvm.opt

import scala.collection.mutable.Map
import scala.collection.JavaConverters._
import java.util.{LinkedHashMap, Collections, Map => JMap}

object LruMap{
  def apply[K,V](maxSize:Int, threadsafe:Boolean): Map[K,V] = {
    require (maxSize > 0)
    val basic = new LruMapImpl[K,V](maxSize)
    val threaded = if (threadsafe) Collections.synchronizedMap(basic) else basic

    threaded.asScala
  }

  private class LruMapImpl[K,V](maxSize: Int) extends LinkedHashMap[K,V] {
    override def removeEldestEntry(eldest: JMap.Entry[K, V]): Boolean = {
      size() > maxSize
    }
  }
}
