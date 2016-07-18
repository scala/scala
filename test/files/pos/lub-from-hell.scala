class Test {
  trait Tree
  def foo(b: Boolean, buf: collection.mutable.ArrayBuffer[Any], acc: StringBuilder) = if (b) buf else acc

  // def bar(b: Boolean,
  //   buf: scala.collection.IndexedSeqLike[Any,Cloneable with Mutable with Equals], 
  //   acc: scala.collection.IndexedSeqLike[_18,scala.collection.mutable.IndexedSeq[_18]] with scala.collection.IndexedSeqLike[_18,IndexedSeq[_18]] forSome { type _18 >: String with Char }
  // ) = if (b) buf else acc


}
