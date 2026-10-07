//> using options -Werror -Wunused:privates -deprecation

class C {
  private final val when = "1.0"

  @deprecated("nope", since = when)
  def f = 42

  def g = f
}
