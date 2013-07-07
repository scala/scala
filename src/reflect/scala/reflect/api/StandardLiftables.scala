package scala.reflect
package api

trait StandardLiftables { self: Universe =>

  private def requireSameUniverse[T](universe: Universe, tp: String, value: T) =
    require(universe eq self, s"Can't lift $tp ${showRaw(value)} from universe ${showRaw(universe)} using lift$tp defined for ${showRaw(self)}.")

  implicit def liftExpr[T <: Expr[_]]: Liftable[T] = new Liftable[T] {
    def apply(universe: Universe, value: T): universe.Tree = {
      requireSameUniverse(universe, "Expr", value)
      value.tree.asInstanceOf[universe.Tree]
    }
  }

  implicit object liftType extends Liftable[Type] {
    def apply(universe: Universe, value: Type): universe.Tree = {
      requireSameUniverse(universe, "Type", value)
      universe.TypeTree(value.asInstanceOf[universe.Type])
    }
  }

  implicit def liftTypeTag[T <: WeakTypeTag[_]]: Liftable[T] = new Liftable[T] {
    def apply(universe: Universe, value: T): universe.Tree = {
      requireSameUniverse(universe, "TypeTag", value)
      universe.TypeTree(value.asInstanceOf[universe.WeakTypeTag[_]].tpe)
    }
  }

  implicit object liftConstant extends Liftable[Constant] {
    def apply(universe: Universe, value: Constant): universe.Tree = {
      requireSameUniverse(universe, "Constant", value)
      universe.Literal(value.asInstanceOf[universe.Constant])
    }
  }
}
