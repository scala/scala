object X { implicit class Y(self: String) }

object T10090 {
  (null: Any) match { case X.Y.Z() => }

  (null: Any) match { case X.Y.Z => }
}
