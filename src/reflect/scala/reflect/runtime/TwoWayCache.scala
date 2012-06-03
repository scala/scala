package scala.reflect
package runtime

/** A cache that maintains a bijection between Java reflection type `J`
 *  and Scala reflection type `S`.
 */
import collection.mutable.HashMap

private[runtime] class TwoWayCache[J, S] {

  private val toScalaMap = new HashMap[J, S]
  private val toJavaMap = new HashMap[S, J]

  def enter(j: J, s: S) = synchronized {
    // debugInfo("cached: "+j+"/"+s)
    toScalaMap(j) = s
    toJavaMap(s) = j
  }

  def toScala(key: J)(body: => S): S = synchronized {
    toScalaMap get key match {
      case Some(v) =>
        v
      case none =>
        val result = body
        enter(key, result)
        result
    }
  }

  def toJava(key: S)(body: => J): J = synchronized {
    toJavaMap get key match {
      case Some(v) =>
        v
      case none =>
        val result = body
        enter(result, key)
        result
    }
  }

  def toJavaOption(key: S)(body: => Option[J]): Option[J] = synchronized {
    toJavaMap get key match {
      case None =>
        val result = body
        for (value <- result) enter(value, key)
        result
      case some => some
    }
  }
}

