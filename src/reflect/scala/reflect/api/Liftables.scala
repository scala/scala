package scala
package reflect
package api

trait Liftables { self: Universe =>

  /** A type class that defines a representation of `T` as a `Tree`.
   *
   *  @see [[http://docs.scala-lang.org/overviews/quasiquotes/lifting.html]]
   */
  trait Liftable[T] {
    def apply(value: T): Tree
  }

  /** Companion to `Liftable` type class that contains standard instances
   *  and provides a helper `apply` method to simplify creation of new ones.
   */
  object Liftable extends StandardLiftableInstances {
    /** A helper method that simplifies creation of `Liftable` instances.
     *  Takes a type and a function that maps that type to a tree representation.
     *
     *  For example to write Liftable for object one might use it like:
     *
     *  {{{
     *  scala> object O
     *
     *  scala> val Oref = symbolOf[O.type].asClass.module
     *
     *  scala> implicit val liftO = Liftable[O.type] { _ => q"$Oref" }
     *
     *  scala> val lifted = q"$O"
     *  lifted: universe.Tree = O
     *  }}}
     *
     *  @see [[http://docs.scala-lang.org/overviews/quasiquotes/lifting.html]]
     */
    def apply[T](f: T => Tree): Liftable[T] =
      new Liftable[T] { def apply(value: T): Tree = f(value) }
  }

  /** A type class that defines a way to extract instance of `T` from a `Tree`.
   *
   *  @see [[http://docs.scala-lang.org/overviews/quasiquotes/unlifting.html]]
   */
  trait Unliftable[T] {
    def unapply(tree: Tree): Option[T]
  }

  /** Companion to `Unliftable` type class that contains standard instances
   *  and provides a helper `apply` method to simplify creation of new ones.
   */
  object Unliftable extends StandardUnliftableInstances {
    /** A helper method that simplifies creation of `Unliftable` instances.
     *  Takes a partial function which is defined on correct representations of `T`
     *  and returns corresponding instances.
     *
     *  For example to extract a reference to an object as object itself:
     *
     *  {{{
     *  scala> object O
     *
     *  scala> val Oref = symbolOf[O.type].asClass.module
     *
     *  scala> implicit val unliftO = Unliftable[O.type] { case t if t.symbol == Oref => O }
     *
     *  scala> val q"${_: O.type}" = q"$Oref"
     *  }}}
     *
     *  @see [[http://docs.scala-lang.org/overviews/quasiquotes/unlifting.html]]
     */
    def apply[T](pf: PartialFunction[Tree, T]): Unliftable[T] = new Unliftable[T] {
      def unapply(value: Tree): Option[T] = pf.lift(value)
    }
  }
}
