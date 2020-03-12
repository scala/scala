// scalac: -Ymacro-annotations -Wunused:params -Werror
@mymacro
class X

object Test {
  println(X.f(123))
}
