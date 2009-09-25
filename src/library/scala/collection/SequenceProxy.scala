/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

/** This trait implements a proxy for sequence objects. It forwards
 *  all calls to a different sequence object.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait SequenceProxy[+A] extends Sequence[A] with SequenceProxyLike[A, Sequence[A]]
