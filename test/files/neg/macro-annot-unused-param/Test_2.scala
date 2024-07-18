//> using options -Ymacro-annotations -Wunused:params -Wmacros:after -Werror
@mymacro
class X

object Test {
  println(X.f(123))
}
