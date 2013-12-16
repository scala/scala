package scala
package reflect
package api

// TODO: needs a Scaladoc
trait Liftables { self: Universe =>

  // TODO: needs a Scaladoc
  trait Liftable[T] {
    def apply(value: T): Tree
  }

  // TODO: needs a Scaladoc
  object Liftable extends StandardLiftableInstances {
    def apply[T](f: T => Tree): Liftable[T] =
      new Liftable[T] { def apply(value: T): Tree = f(value) }
  }

  // TODO: needs a Scaladoc
  trait Unliftable[T] {
    def unapply(tree: Tree): Option[T]
  }

  // TODO: needs a Scaladoc
  object Unliftable extends StandardUnliftableInstances {
    def apply[T](pf: PartialFunction[Tree, T]): Unliftable[T] = new Unliftable[T] {
      def unapply(value: Tree): Option[T] = pf.lift(value)
    }
  }
}
