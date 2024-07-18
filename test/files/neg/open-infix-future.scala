//> using options -Xsource:3
//

open trait A // error
open object B // error

class C {
  infix val a: Int = 1 // error
  infix var b: Int = 1 // error

  open type D // error

  def foo: Unit = {
    open class E // error
    open def bla(y: Int) = y // error
  }
}
