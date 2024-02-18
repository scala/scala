//> using options -Ymacro-annotations -Werror -Wmacros:default -Wunused:params
@mymacro
class X

object Test {
  println(X.f(123))
}
