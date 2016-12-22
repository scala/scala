package scala.reflect
package runtime

import java.lang.ref.WeakReference
import java.util.{Collections, WeakHashMap}

import scala.collection.convert.decorateAsScala._

/** A cache that maintains a bijection between Java reflection type `J`
 *  and Scala reflection type `S`.
 *
 *  The cache is two-way weak (i.e. is powered by weak references),
 *  so that neither Java artifacts prevent Scala artifacts from being garbage collected,
 *  nor the other way around.
 */
private[runtime] trait TwoWayCaches { self: SymbolTable =>
  class TwoWayCache[J, S] {

    private val toScalaMap = Collections.synchronizedMap(new WeakHashMap[J, WeakReference[S]]()).asScala
    private val toJavaMap = Collections.synchronizedMap(new WeakHashMap[S, WeakReference[J]]()).asScala

    def enter(j: J, s: S) = {
      // debugInfo("cached: "+j+"/"+s)
      toScalaMap(j) = new WeakReference(s)
      toJavaMap(s) = new WeakReference(j)
    }

    private object SomeRef {
      def unapply[T](optRef: Option[WeakReference[T]]): Option[T] =
        if (optRef.nonEmpty) {
          Option(optRef.get.get)
        } else None
    }

    def syncToScala(key: J)(body: => S): S = gilSynchronized {
      toScalaMap get key match {
        case SomeRef(v) =>
          v
        case _ =>
          val result = body
          enter(key, result)
          result
      }
    }

    def toScala(key: J)(body: => S): S = {
      toScalaMap get key match {
        case SomeRef(v) =>
          v
        case _ =>
          syncToScala(key)(body)
      }
    }

    def syncToJava(key: S)(body: => J): J = gilSynchronized {
      toJavaMap get key match {
        case SomeRef(v) =>
          v
        case _ =>
          val result = body
          enter(result, key)
          result
      }
    }

    def toJava(key: S)(body: => J): J = {
      toJavaMap get key match {
        case SomeRef(v) =>
          v
        case _ =>
          syncToJava(key)(body)
      }
    }

    def syncToJavaOption(key: S)(body: => Option[J]): Option[J] = gilSynchronized {
      toJavaMap get key match {
        case SomeRef(v) =>
          Some(v)
        case _ =>
          val result = body
          for (value <- result) enter(value, key)
          result
      }
    }

    def toJavaOption(key: S)(body: => Option[J]): Option[J] = {
      toJavaMap get key match {
        case SomeRef(v) =>
          Some(v)
        case _ =>
          syncToJavaOption(key)(body)
      }
    }
  }
}


