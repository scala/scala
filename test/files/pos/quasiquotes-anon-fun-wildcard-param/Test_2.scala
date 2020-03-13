// scalac: -Wunused:params -Wmacros:after -Werror
object Test extends App {
  println(Macros.f(123))
}
