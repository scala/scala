import scala.reflect.{ClassTag, classTag}

class SparseArray2[@specialized(Int) T:ClassTag](val maxSize: Int, initialLength:Int = 3) {
  private var data = new Array[T](initialLength);
  private var index = new Array[Int](initialLength);

  // comment out to compile correctly
  data.length + 3;
}