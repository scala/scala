object Test extends Application{
  def rangeForeach(range : Range) = {
    val buffer = new scala.collection.mutable.ListBuffer[Int];
    range.foreach(buffer += _);
    assert(buffer.toList == range.iterator.toList, buffer.toList+"/"+range.iterator.toList)
  }

  rangeForeach(1 to 10);
  rangeForeach(1 until 10);
  rangeForeach(10 to 1 by -1);
  rangeForeach(10 until 1 by -1);
  rangeForeach(10 to 1 by -3);
  rangeForeach(10 until 1 by -3);
}
