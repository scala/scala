/* NSC -- new Scala compiler
 * Copyright 2010-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package collection
package immutable

trait StreamView[+A, +Coll] extends StreamViewLike[A, Coll, StreamView[A, Coll]] { }
