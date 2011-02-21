object Test extends App{
  import scala.collection.immutable.IntMap;

  val it = IntMap(8 -> 2, 11 -> 3, 1 -> 2, 7 -> 13);

  assert(it.firstKey == 1);
  assert(it.lastKey == 11);
}
