///> using options -Vdebug -Vlog:typer -Werror

trait T

class C(implicit t: T) {
  def f: T = implicitly[T]

  def g(t: String): T = implicitly[T]
}
