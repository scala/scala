package scala.reflect
package api

import language.experimental.macros

trait Quasiquotes { self: Universe =>

  // implementation is hardwired to `dispatch` method of `scala.tools.reflect.quasiquotes.Quasiquotes`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  implicit class Quasiquote(ctx: StringContext) {
    protected trait api {
      def apply(args: Any*): Any = macro ???
      def unapply(subpatterns: Any*): Option[Any] = macro ???
    }
    object q extends api
    object tq extends api
    object cq extends api
    object pq extends api
  }
}
