/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.backend.jvm.opt

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.{LinkedHashMap, Map => JMap}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object FifoCache {
  def apply[K,V](maxSize: Int, threadsafe: Boolean): mutable.Map[K,V] = {
    require(maxSize > 0)
    if (threadsafe) new ConcFifoCache(maxSize) else new FifoCache[K, V](maxSize).asScala
  }

  private class FifoCache[K, V](maxSize: Int) extends LinkedHashMap[K,V] {
    override def removeEldestEntry(eldest: JMap.Entry[K, V]): Boolean = {
      size() > maxSize
    }
  }

  private class ConcFifoCache[K, V](maxSize: Int) extends mutable.Map[K,V] {
    private val cache: ConcurrentHashMap[K, V] = new ConcurrentHashMap()
    private val queue: ConcurrentLinkedQueue[K] = new ConcurrentLinkedQueue()

    def get(key: K): Option[V] = Option(cache.get(key))

    def subtractOne(key: K): this.type = {
      cache.remove(key)
      queue.remove(key)
      this
    }

    def addOne(elem: (K, V)): this.type = {
      while (cache.size() >= maxSize) {
        val oldest = queue.poll()
        if (oldest != null) cache.remove(oldest)
      }
      queue.add(elem._1)
      cache.put(elem._1, elem._2)
      this
    }

    def iterator: Iterator[(K, V)] = cache.entrySet.iterator.asScala.map(e => (e.getKey, e.getValue))
  }
}
