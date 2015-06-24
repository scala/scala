class DelambdafyPatterns {
  def bar: Unit = ()
  def wildcardPatternInTryCatch: Unit => Unit = (x: Unit) =>
    // patterns in try..catch are preserved so we need to be
    // careful when it comes to free variable detection
    // in particular a is _not_ free variable, also the
    // `_` identifier has no symbol attached to it
    try bar catch {
      case a@(_:java.lang.reflect.InvocationTargetException) =>
        // refer to a so we trigger a bug where a is considered
        // to be a free variable for enclosing lambda
        val b = a
        ()
    }
}
