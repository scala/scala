package scala.collection.parallel.benchmarks.arrays







object Arrays {
  
  @inline def genericApply[T](xs: Array[T], idx: Int): T = xs.asInstanceOf[AnyRef] match {
    case x: Array[AnyRef] => x(idx).asInstanceOf[T]
    case _ => genericApplyNotAnyRef(xs, idx)
  }
  
  @noinline private def genericApplyNotAnyRef[T](xs: Array[T], idx: Int): T = xs.asInstanceOf[AnyRef] match {
    case x: Array[Int] => x(idx).asInstanceOf[T]
    case x: Array[Double] => x(idx).asInstanceOf[T]
    case x: Array[Long] => x(idx).asInstanceOf[T]
    case x: Array[Float] => x(idx).asInstanceOf[T]
    case x: Array[Char] => x(idx).asInstanceOf[T]
    case x: Array[Byte] => x(idx).asInstanceOf[T]
    case x: Array[Short] => x(idx).asInstanceOf[T]
    case x: Array[Boolean] => x(idx).asInstanceOf[T]
    case x: Array[Unit] => x(idx).asInstanceOf[T]
    case null => throw new NullPointerException
  }
  
  @inline def apply(xs: AnyRef, idx: Int): Any = xs match {
    case x: Array[AnyRef] => x(idx).asInstanceOf[Any]
    case _ => applyNotAnyRef(xs, idx)
  }
  
  @noinline private def applyNotAnyRef(xs: AnyRef, idx: Int): Any = xs match {
    case x: Array[Int] => x(idx).asInstanceOf[Any]
    case x: Array[Double] => x(idx).asInstanceOf[Any]
    case x: Array[Long] => x(idx).asInstanceOf[Any]
    case x: Array[Float] => x(idx).asInstanceOf[Any]
    case x: Array[Char] => x(idx).asInstanceOf[Any]
    case x: Array[Byte] => x(idx).asInstanceOf[Any]
    case x: Array[Short] => x(idx).asInstanceOf[Any]
    case x: Array[Boolean] => x(idx).asInstanceOf[Any]
    case x: Array[Unit] => x(idx).asInstanceOf[Any]
    case null => throw new NullPointerException
  }
  
}
















