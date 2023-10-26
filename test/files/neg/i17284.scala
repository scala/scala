//> using options -Werror

class C {
  def literal = 451.synchronized {} // error

  def integral = {
    val x = 451
    x.synchronized {} // error
  }

  def boxed = {
    val x: Integer = 451
    x.synchronized {} // error
  }

  def bool = true.synchronized {} // error

  // hard error
  //def unit = ().synchronized {} // error
}
