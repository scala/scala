object Unchecked {
  (null: Any) match {
    case _: Some[t] =>

      // t is a fresh pattern type variable, despite our attempts to
      // backtick our way to the enclosing `t`. Under this interpretation,
      // the absence of an unchecked warning is expected.
      (null: Any) match {
        case _: Some[t] => // no warn
      }
      (null: Any) match {
        case _: Some[`t`] => // no warn
      }

      // here we correctly issue an unchecked warning
      type T = t
      (null: Any) match {
        case _: Some[T] => // warn
      }
  }
}
