/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** <p>
 *    A base class for static attributes. These are available
 *    to the Scala type checker, even across different compilation units.
 *  </p>
 *
 *  @deprecated  use <a href="StaticAnnotation.html"
 *               target="contentFrame">StaticAnnotation</a> instead
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 */
@deprecated
trait StaticAttribute extends Attribute {}
