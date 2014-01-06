package scala.reflect
package api

trait Quasiquotes { self: Universe =>

  // implementation is hardwired to `dispatch` method of `scala.tools.reflect.quasiquotes.Quasiquotes`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  implicit class Quasiquote(ctx: StringContext) {
    protected trait api {
      def apply[T](args: T*): Any = macro ???
      def unapply(scrutinee: Any): Any = macro ???
    }
    object q extends api
    object tq extends api
    object cq extends api
    object pq extends api
    object fq extends api
  }
}
