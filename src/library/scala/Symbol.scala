/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

// jcl disabled for now; go directly to java collections.
//import scala.collection.jcl

//private[scala] object internedSymbols extends jcl.WeakHashMap[String, ref.WeakReference[Symbol]]
private[scala] object internedSymbols
  extends java.util.WeakHashMap[String, java.lang.ref.WeakReference[Symbol]]

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

object Symbol {

  /** <p>
   *    Makes this symbol into a unique reference.
   *  </p>
   *  <p>
   *    If two interened symbols are equal (i.e. they have the same name)
   *    then they must be identical (wrt reference equality).
   *  </p>
   *
   *  @return the unique reference to this string.
   */
  def apply(name: String): Symbol = internedSymbols.synchronized {
//    internedSymbols.get(name).flatMap(_.get) match {
//    case Some(sym) => sym
//    case _ =>
//      val sym = new Symbol(name)
//      internedSymbols(name) = new ref.WeakReference(sym)
//      sym
    var sym: Symbol = null
    val ref = internedSymbols.get(name)
    if (ref != null) sym = ref.get
    if (sym == null) {
      sym = new Symbol(name)
      internedSymbols.put(name, new java.lang.ref.WeakReference(sym))
    }
    sym
  }

  def unapply(other: Symbol): Option[String] = Some(other.name)
}
