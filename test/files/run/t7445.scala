import scala.collection.immutable.ListMap

object Test extends App {
	val a = ListMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5);
	require(a.tail == ListMap(2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5));
}
