//> using options -opt:inline:** -Wopt -Werror
package test

object A {

  private var x: Int = 0

  @inline def actOnX(f: Int => Int) = { x = f(x) }
}

object B {

  private[this] var x: Int = 0

  @inline def actOnX(f: Int => Int) = { x = f(x) }
}
