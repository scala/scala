//> using options -Ymacro-annotations -Werror -Wmacros:before -Wunused:params
@mymacro
class X

object Test {
  println(X.f(123))
}
