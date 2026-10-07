//> using options -deprecation -Werror

@Deprecated
class Annotee

class C {
  def b = true

  def f = new Annotee // error

  @deprecated
  type TR = annotation.tailrec

  @TR
  final def g: String = if (b) g else "" // error

  def h = new TR // error
}
