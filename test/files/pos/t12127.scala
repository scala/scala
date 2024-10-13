
//> using options -Werror -Wunused

class C {
  def f(x: Any): "hi" = x match { case s @ "hi" => s } // was: Error while emitting (in backend)
  def g(x: Any): String = x match { case s @ "hi" => s }
  def h(x: Any): String = x match { case s: String => s }

  def check(x: Any): "bi" = x match { case s @ "hi" => "bi" }
}
