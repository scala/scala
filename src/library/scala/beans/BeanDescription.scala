/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.beans

/** Provides a short description that will be included when generating
 *  bean information. This annotation can be attached to the bean itself,
 *  or to any member.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
class BeanDescription(val description: String) extends annotation.Annotation

