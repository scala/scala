/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;

/** Observable update events are issued by observable collection classes
 *  whenever a data structure is changed. Class <code>ObservableUpdate</code>
 *  has several subclasses for the various kinds of events: <code>Modification</code>
 *  <code>Removal</code>, <code>Insertion</code>, and <code>Reset</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait ObservableUpdate[+A];
