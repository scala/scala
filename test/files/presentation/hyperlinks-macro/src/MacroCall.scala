object Test {
  def foo = 0

  scala.reflect.runtime.universe.reify {
    foo/*#*/
  }

  identity {
    foo/*#*/
  }
}
// Currently, the hyperlink within the argument to the macro `reify` does not resolve correctly.
