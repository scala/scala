/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** <p>
 *    This class provides a simple way to get unique objects for
 *    equal strings. Since symbols are interned, they can be compared using
 *    reference equality. Instances of
 *    <code>Symbol</code> can be created easily with Scala's built-in
*     quote mechanism.
 *  </p>
 *  <p>
 *    For instance, the <a href="http://scala-lang.org/" target="_top">Scala</a>
 *    term <code>'mysym</code> will invoke the constructor of the
 *    <code>Symbol</code> class in the following way:
 *    <code>Symbol("mysym")</code>.
 *  </p>
 *
 *  @author  Martin Odersky, Iulian Dragos
 *  @version 1.8
 */
@serializable
final class Symbol private (val name: String) {
  /** Converts this symbol to a string.
   */
  override def toString(): String = "'" + name

  @throws(classOf[java.io.ObjectStreamException])
  private def readResolve(): Any = Symbol.apply(name)
}

object Symbol extends UniquenessCache[String, Symbol]
{
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

/** This is private so it won't appear in the library API, but
  * abstracted to offer some hope of reusability.  */
private[scala] abstract class UniquenessCache[K, V >: Null]
{
  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V
  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      rlock.lock
      try {
        val reference = map get name
        if (reference == null) null
        else reference.get  // will be null if we were gc-ed
      }
      finally rlock.unlock
    }
    def updateCache(): V = {
      wlock.lock
      try {
        val res = cached()
        if (res != null) res
        else {
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }
  def unapply(other: V): Option[K] = keyFromValue(other)
}
