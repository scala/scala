trait ConfigurableDefault[@specialized V] {
  def fillArray(arr: Array[V], v: V) = (arr: Any) match {
    case x: Array[Int]  => null
    case x: Array[Long] => v.asInstanceOf[Long]
  }
}
