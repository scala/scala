trait Baz {
  type Type >: Null

  case class HoleType(a: String, b: String, c: String)
  object HoleType { def unapply(tpe: Type): Option[HoleType] = ??? }

  (null: Type) match { case HoleType(holeTpe) => holeTpe }
}
