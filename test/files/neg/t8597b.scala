object Unchecked {
  (null: Any) match {
    case _: Some[t] =>

      // t is a fresh pattern type variable, despite our attempts to
      // backtick our way to the enclosing `t`. Under this interpretation,
      // the absense of an uchecked warning is expected.
      (null: Any) match {
        case _: Some[t] =>
      }
      (null: Any) match {
        case _: Some[`t`] =>
      }

      // here we correctly issue an unchecked warning
      type T = t
      (null: Any) match {
        case _: Some[T] =>
      }
  }
}
