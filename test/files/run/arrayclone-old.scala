object Test extends App{
  BooleanArrayClone;
  ByteArrayClone;
  ShortArrayClone;
  CharArrayClone;
  IntArrayClone;
  LongArrayClone;
  FloatArrayClone;
  DoubleArrayClone;
  ObjectArrayClone;
  PolymorphicArrayClone;
}

object BooleanArrayClone{
  val it : Array[Boolean] = Array(true, false);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = false;
  assert(it(0) == true)
}

object ByteArrayClone{
  val it : Array[Byte] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object ShortArrayClone{
  val it : Array[Short] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object CharArrayClone{
  val it : Array[Char] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object IntArrayClone{
  val it : Array[Int] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object LongArrayClone{
  val it : Array[Long] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object FloatArrayClone{
  val it : Array[Float] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object DoubleArrayClone{
  val it : Array[Double] = Array(1, 0);
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = 0;
  assert(it(0) == 1)
}

object ObjectArrayClone{
  val it : Array[String] = Array("1", "0");
  val cloned = it.clone();
  assert(cloned.sameElements(it));
  cloned(0) = "0";
  assert(it(0) == "1")
}

object PolymorphicArrayClone{
  def testIt[T](it : Array[T], one : T, zero : T) = {
    val cloned = it.clone();
    assert(cloned.sameElements(it));
    cloned(0) = zero;
    assert(it(0) == one)
  }

  testIt(Array("one", "two"), "one", "two");

  class Mangler[T: Manifest](ts : T*){
    // this will always be a BoxedAnyArray even after we've unboxed its contents.
    val it = ts.toArray[T];
  }

  val mangled = new Mangler[Int](0, 1);

  val y : Array[Int] = mangled.it; // make sure it's unboxed

  testIt(mangled.it, 0, 1);
}
