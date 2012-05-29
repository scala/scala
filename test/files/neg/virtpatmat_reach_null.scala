sealed abstract class Const {
  final def excludes(other: Const) =
    (this, other) match {
      case (_, NullConst)                 =>
      case (NullConst, _)                 =>
      case (_: ValueConst, _: ValueConst) =>
      case (_: ValueConst, _: TypeConst)  =>
      case (_: TypeConst,  _: ValueConst) =>
      case (_: TypeConst,  _: TypeConst)  =>
      case (null, _)                      =>
      case (_, null)                      =>
      case null                           =>
      case _                              =>  // unreachable
    }
}

sealed class TypeConst extends Const
sealed class ValueConst extends Const
case object NullConst extends Const
