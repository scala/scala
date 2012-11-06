/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

/** This trait implements a proxy for sequence objects. It forwards
 *  all calls to a different sequence object.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait SeqProxy[+A] extends Seq[A] with SeqProxyLike[A, Seq[A]]
