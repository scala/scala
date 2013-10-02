object Test {

  @inline final def mbarray_apply_minibox(array: Any, tag: Byte): Long =
    if (tag == 0) {
      array.asInstanceOf[Array[Long]](0)
    } else
      array.asInstanceOf[Array[Byte]](0).toLong

  def crash_method(): Unit =
    mbarray_apply_minibox(null, 0)
}
