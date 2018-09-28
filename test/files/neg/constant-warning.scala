// scalac: -Xlint:constant -Xfatal-warnings
object Test {
  val fails = 1 + 2 / (3 - 2 - 1)
}
