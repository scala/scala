import collection.{JavaConversions, mutable}
import JavaConversions._
import java.util.concurrent.ConcurrentHashMap

object Foo {
  def buildCache2_9_simple[K <: AnyRef, V <: AnyRef]: mutable.ConcurrentMap[K, V] =
    asScalaConcurrentMap(new ConcurrentHashMap())

  def buildCache2_9_implicit[K <: AnyRef, V <: AnyRef]: mutable.ConcurrentMap[K, V] =
    new ConcurrentHashMap[K, V]()
}
