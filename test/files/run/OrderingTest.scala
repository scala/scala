object Test extends App {
  def test[T](t1 : T, t2 : T)(implicit ord : Ordering[T]) = {
    val cmp = ord.compare(t1, t2);
    val cmp2 = ord.compare(t2, t1);

    assert((cmp == 0) == (cmp2 == 0))
    assert((cmp > 0) == (cmp2 < 0))
    assert((cmp < 0) == (cmp2 > 0))
  }

  def testAll[T](t1 : T, t2 : T)(implicit ord : Ordering[T]) = {
    assert(ord.compare(t1, t2) < 0)
    test(t1, t2);
    test(t1, t1);
    test(t2, t2);
  }

  assert(Ordering[String].compare("australopithecus", "brontausaurus") < 0)
  // assert(Ordering[Unit].compare((), ()) == 0)

  testAll("bar", "foo");
  testAll[Byte](0, 1);
  testAll(false, true)
  testAll(1, 2);
  testAll(1.0, 2.0);
  testAll(None, Some(1));
  testAll[Iterable[Int]](List(1), List(1, 2));
  testAll[Iterable[Int]](List(1, 2), List(2));
  testAll((1, "bar"), (1, "foo"))
  testAll((1, "foo"), (2, "bar"))

  // sortBy
  val words = "The quick brown fox jumped over the lazy dog".split(' ')
  val result = words.sortBy(x => (x.length, x.head))
  assert(result sameElements Array[String]("The", "dog", "fox", "the", "lazy", "over", "brown", "quick", "jumped"))
}
