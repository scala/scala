package scala.reflect
package api

trait QuasiQuotes { self: Universe =>

  // implementation is hardwired to methods of `scala.tools.reflect.Quasiquotes`
  // using the mechanism implemented in `scala.tools.reflect.FastTrack`
  implicit class QuasiQuote(ctx: StringContext) {
    object q {
      def apply(args: Any*): Any = ??? // macro
      def unapply(tree: Any): Option[Any] = ??? // macro
    }
    object tq {
      def apply(args: Any*): Any = ??? // macro
      def unapply(tree: Any): Option[Any] = ??? // macro
    }
  }

  implicit object liftType extends Liftable[Type] {
    def apply(universe: Universe, value: Type): universe.Tree = {
      require(universe eq self, s"Can't lift Type ${showRaw(value)} from universe ${showRaw(universe)} using liftType defined for ${showRaw(self)}.")
      universe.TypeTree(value.asInstanceOf[universe.Type])
    }
  }

  implicit object liftSymbol extends Liftable[Symbol] {
    def apply(universe: Universe, value: Symbol): universe.Tree = {
      require(universe eq self, s"Can't lift Symbol ${showRaw(value)} from universe ${showRaw(universe)} using liftSymbol defined for ${showRaw(self)}.")
      universe.Ident(value.asInstanceOf[universe.Symbol])
    }
  }
}
