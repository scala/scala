// scalac: -opt:l:inline -opt-inline-from:** -Xfatal-warnings -opt-warnings
package test

object A {

  private var x: Int = 0

  @inline def actOnX(f: Int => Int) = {
    x = f(x)
  }
}
