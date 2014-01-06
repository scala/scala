/*
 * filter: It would fail on the following input
 */
object Test extends App {
  List(List(1), List(2)) match { case x :: (y :: Nil) :: Nil => println(y) }
}
