package scala.collection

package object immutable {
  @deprecated("Stream has been renamed to LazyList", "2.12")
  type Stream[+A] = LazyList[A]
  @deprecated("Stream has been renamed to LazyList", "2.12")
  val  Stream     = LazyList

  @deprecated("StreamView has been renamed to LazyListView", "2.12")
  type StreamView[+A, +Coll] = LazyListView[A, Coll]

  @deprecated("StreamViewLike has been renamed to LazyListViewLike", "2.12")
  type StreamViewLike[+A, +Coll, +This <: LazyListView[A, Coll] with LazyListViewLike[A, Coll, This]] = LazyListViewLike[A, Coll, This]
}
