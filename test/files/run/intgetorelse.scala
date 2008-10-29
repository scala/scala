object Test extends Application{
  import scala.collection.immutable.{IntMap, LongMap}


  val t1 = IntMap(1 -> 2, 2 -> 3, 3 -> 4, 5 -> 7);
  assert(t1.getOrElse(1, 3) == 2)
  assert(t1.getOrElse(9, 3) == 3)


  val t2 = LongMap(1L -> 2, 2L -> 3, 3L -> 4, 5L -> 7);
  assert(t2.getOrElse(1, 3L) == 2L)
  assert(t2.getOrElse(9, 3L) == 3L)


}
