object Test {
  M.cto // error
  M.m(M.cto, ()) // error
  M.m((), M.cto) // okay
  M.cto // error

  locally {
    val expr = scala.reflect.runtime.universe.reify(2)
    val splice = expr.splice
    val value = expr.value
  }
}
