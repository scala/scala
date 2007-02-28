/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** <p>
 *    A base class for classfile attributes. These are stored as
 *    Java annotations in classfiles.
 *  </p>
 *
 *  @deprecated  use ClassfileAnnotation instead
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 */
@deprecated
trait ClassfileAttribute extends Attribute {}
