/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: StaticAnnotation.scala 9916 2007-02-07 13:07:33 +0000 (Wed, 07 Feb 2007) michelou $


package scala

/** <p>
 *    A base class for static annotations. These are available
 *    to the Scala type checker, even across different compilation units.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 */
trait StaticAnnotation extends Annotation {}
