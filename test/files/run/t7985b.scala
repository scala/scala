class a { type X = Int }

object Test extends App {
  Array(1) match { case _: Array[a#X] => }
}
