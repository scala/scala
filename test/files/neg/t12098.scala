//xlint supersedes default Wconf setting, which is ws warn-summary for deprecation
//> using options -Werror -Wconf:cat=lint-missing-interpolator:ws -Xlint

class C(i: Int) {
  def p() = println("hi $i")

  @deprecated("don't", since="1.0") def f() = 42

  def g() = f()
}
