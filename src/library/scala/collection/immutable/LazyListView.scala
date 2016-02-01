package scala
package collection
package immutable

trait LazyListView[+A, +Coll] extends LazyListViewLike[A, Coll, LazyListView[A, Coll]] { }
