/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import scala.collection.jcl

private[scala] object internedSymbols extends jcl.WeakHashMap[Symbol, ref.WeakReference[Symbol]]

/** <p>
 *    This class provides a simple way to get unique objects for
 *    equal strings. By default, symbols use string equality for
 *    equality testing, but interned symbols can be compared using
 *    reference equality for the same effect. Instances of
 *    <code>Symbol</code> can be created easily with Scala's built-in
*     quote mechanism.
 *  </p>
 *  <p>
 *    For instance, the <a href="http://scala-lang.org/" target="_top">Scala</a>
 *    term <code>'mysym</code> will invoke the constructor of the
 *    <code>Symbol</code> class in the following way:
 *    <code>new Symbol("mysym").intern</code>.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 1.7, 08/12/2003
 */
final case class Symbol(name: String) {

  /** Converts this symbol to a string.
   */
  override def toString(): String = {
    "'" + name
  }

  /** <p>
   *    Makes this symbol into a unique reference.
   *  </p>
   *  <p>
   *    If two interened symbols are equal (i.e. they have the same name)
   *    then they must be identical (wrt reference equality).
   *  </p>
   *
   *  @return the unique reference to this symbol.
   */
  def intern: Symbol = internedSymbols.synchronized {
    internedSymbols.get(this).map(_.get).getOrElse(None) match {
    case Some(sym) => sym
    case _ =>
      internedSymbols(this) = new ref.WeakReference(this); this
  } }
}
