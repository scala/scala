object Test extends App{
  import scala.collection.immutable.LongMap;

  val it = LongMap(8L -> 2, 11L -> 3, 1L -> 2, 7L -> 13);

  assert(it.firstKey == 1L);
  assert(it.lastKey == 11L);
}
