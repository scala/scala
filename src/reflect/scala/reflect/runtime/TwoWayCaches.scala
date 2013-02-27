package scala.reflect
package runtime

import scala.collection.mutable.WeakHashMap
import java.lang.ref.WeakReference

/** A cache that maintains a bijection between Java reflection type `J`
 *  and Scala reflection type `S`.
 *
 *  The cache is two-way weak (i.e. is powered by weak references),
 *  so that neither Java artifacts prevent Scala artifacts from being garbage collected,
 *  nor the other way around.
 */
private[runtime] trait TwoWayCaches { self: SymbolTable =>
  class TwoWayCache[J, S] {

    private val toScalaMap = new WeakHashMap[J, WeakReference[S]]
    private val toJavaMap = new WeakHashMap[S, WeakReference[J]]

    def enter(j: J, s: S) = gilSynchronized {
      // debugInfo("cached: "+j+"/"+s)
      toScalaMap(j) = new WeakReference(s)
      toJavaMap(s) = new WeakReference(j)
    }

    private object SomeRef {
      def unapply[T](optRef: Option[WeakReference[T]]): Option[T] =
        if (optRef.nonEmpty) {
          val result = optRef.get.get
          if (result != null) Some(result) else None
        } else None
    }

    def toScala(key: J)(body: => S): S = gilSynchronized {
      toScalaMap get key match {
        case SomeRef(v) =>
          v
        case _ =>
          val result = body
          enter(key, result)
          result
      }
    }

    def toJava(key: S)(body: => J): J = gilSynchronized {
      toJavaMap get key match {
        case SomeRef(v) =>
          v
        case _ =>
          val result = body
          enter(result, key)
          result
      }
    }

    def toJavaOption(key: S)(body: => Option[J]): Option[J] = gilSynchronized {
      toJavaMap get key match {
        case SomeRef(v) =>
          Some(v)
        case _ =>
          val result = body
          for (value <- result) enter(value, key)
          result
      }
    }
  }
}

