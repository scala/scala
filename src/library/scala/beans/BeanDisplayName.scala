/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.beans

/** Provides a display name when generating bean information. This
 *  annotation can be attached to the bean itself, or to any member.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
class BeanDisplayName(val name: String) extends scala.annotation.Annotation

