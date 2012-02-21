abstract sealed class ArgNumber
case object IsList extends ArgNumber
case object ArgNumber

object Test extends App {
  println(IsList)
}
