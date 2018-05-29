
@kase class KaseClassPreToplevelNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
object KaseClassPreToplevelPrecomp
@kase class KaseClassPreToplevelPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@kase class KaseClassPreToplevelPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
object KaseClassPreToplevelPostcomp

class KaseClass {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val objects = scala.collection.mutable.ListBuffer[Any]()
  objects += KaseClassPreToplevelNocomp(42)(true)
  objects += KaseClassPreToplevelPrecomp(42)(true)
  objects += KaseClassPreToplevelPostcomp(42)(true)
  objects += KaseClassPostToplevelNocomp(42)(true)
  objects += KaseClassPostToplevelPrecomp(42)(true)
  objects += KaseClassPostToplevelPostcomp(42)(true)

  @kase class KaseClassPreMemberNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  object KaseClassPreMemberPrecomp
  @kase class KaseClassPreMemberPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @kase class KaseClassPreMemberPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  object KaseClassPreMemberPostcomp
  objects += KaseClassPreMemberNocomp(42)(true)
  objects += KaseClassPreMemberPrecomp(42)(true)
  objects += KaseClassPreMemberPostcomp(42)(true)
  objects += KaseClassPostMemberNocomp(42)(true)
  objects += KaseClassPostMemberPrecomp(42)(true)
  objects += KaseClassPostMemberPostcomp(42)(true)
  @kase class KaseClassPostMemberNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  object KaseClassPostMemberPrecomp
  @kase class KaseClassPostMemberPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  @kase class KaseClassPostMemberPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
  object KaseClassPostMemberPostcomp

  // @Test
  def combo: Unit = {
    @kase class KaseClassPreLocalNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    object KaseClassPreLocalPrecomp
    @kase class KaseClassPreLocalPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @kase class KaseClassPreLocalPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    object KaseClassPreLocalPostcomp
    objects += KaseClassPreLocalNocomp(42)(true)
    objects += KaseClassPreLocalPrecomp(42)(true)
    objects += KaseClassPreLocalPostcomp(42)(true)
    objects += KaseClassPostLocalNocomp(42)(true)
    objects += KaseClassPostLocalPrecomp(42)(true)
    objects += KaseClassPostLocalPostcomp(42)(true)
    @kase class KaseClassPostLocalNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    object KaseClassPostLocalPrecomp
    @kase class KaseClassPostLocalPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    @kase class KaseClassPostLocalPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
    object KaseClassPostLocalPostcomp

    assertEquals(objects.mkString("\n"), """
      |KaseClassPreToplevelNocomp(42,2)
      |KaseClassPreToplevelPrecomp(42,2)
      |KaseClassPreToplevelPostcomp(42,2)
      |KaseClassPostToplevelNocomp(42,2)
      |KaseClassPostToplevelPrecomp(42,2)
      |KaseClassPostToplevelPostcomp(42,2)
      |KaseClassPreMemberNocomp(42,2)
      |KaseClassPreMemberPrecomp(42,2)
      |KaseClassPreMemberPostcomp(42,2)
      |KaseClassPostMemberNocomp(42,2)
      |KaseClassPostMemberPrecomp(42,2)
      |KaseClassPostMemberPostcomp(42,2)
      |KaseClassPreLocalNocomp(42,2)
      |KaseClassPreLocalPrecomp(42,2)
      |KaseClassPreLocalPostcomp(42,2)
      |KaseClassPostLocalNocomp(42,2)
      |KaseClassPostLocalPrecomp(42,2)
      |KaseClassPostLocalPostcomp(42,2)
    """.trim.stripMargin)
  }
}

@kase class KaseClassPostToplevelNocomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
object KaseClassPostToplevelPrecomp
@kase class KaseClassPostToplevelPrecomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
@kase class KaseClassPostToplevelPostcomp[T](x: T, y: Int = 2)(z: Boolean, w: String = "")
object KaseClassPostToplevelPostcomp
