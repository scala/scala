/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ClassfileAnnotation.scala 9916 2007-02-07 13:07:33 +0000 (Wed, 07 Feb 2007) michelou $


package scala

/** <p>
 *    A base class for classfile annotations. These are stored as
 *    Java annotations in classfiles.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 */
trait ClassfileAnnotation extends Annotation {}
