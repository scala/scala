package scala
package collection
package immutable

trait StreamView[+A, +Coll] extends StreamViewLike[A, Coll, StreamView[A, Coll]] { }
