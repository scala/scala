/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;

/** This observable update refers to destructive modification operations
 *  of elements from collection classes.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
case class Modification[A](old: A, nu: A) extends ObservableUpdate[A];
