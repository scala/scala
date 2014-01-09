object Test {
  def foo = 0

  scala.reflect.runtime.universe.reify {
    foo/*#*/
  }

  identity {
    foo/*#*/
  }
}
