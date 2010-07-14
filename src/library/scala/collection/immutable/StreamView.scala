package scala.collection
package immutable



import scala.collection.generic.CanBuildFrom





trait StreamView[+A, +Coll] extends StreamViewLike[A, Coll, StreamView[A, Coll]]
