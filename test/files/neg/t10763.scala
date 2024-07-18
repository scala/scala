//> using options -Werror -Wunused

object Test extends App {
  // cf test/files/pos/t10763.scala test/files/run/t11938.scala
  // withFilter is exempt from warning but foreach is not.
  for (x @ 1 <- List(1.0)) ()
}
