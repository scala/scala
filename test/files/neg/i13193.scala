//> using options -Werror -deprecation
class C {
  def f(@deprecatedName(name = Symbol("x"), since="forever") a: Int): Int = a
  def g(@deprecatedName(since = "forever", name = Symbol("x")) a: Int): Int = a
}
