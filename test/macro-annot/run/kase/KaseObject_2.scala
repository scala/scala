@kase object KaseObjectPreToplevelNocomp
class KaseObjectPreToplevelPrecomp
@kase object KaseObjectPreToplevelPrecomp
@kase object KaseObjectPreToplevelPostcomp
class KaseObjectPreToplevelPostcomp

class KaseObject {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  val objects = scala.collection.mutable.ListBuffer[Any]()
  objects += KaseObjectPreToplevelNocomp
  objects += KaseObjectPreToplevelPrecomp
  objects += KaseObjectPreToplevelPostcomp
  objects += KaseObjectPostToplevelNocomp
  objects += KaseObjectPostToplevelPrecomp
  objects += KaseObjectPostToplevelPostcomp

  // TODO: doesn't work in sbt, though does work in the command line
  // @kase object KaseObjectPreMemberNocomp
  // class KaseObjectPreMemberPrecomp
  // @kase object KaseObjectPreMemberPrecomp
  // @kase object KaseObjectPreMemberPostcomp
  // class KaseObjectPreMemberPostcomp
  // objects += KaseObjectPreMemberNocomp
  // objects += KaseObjectPreMemberPrecomp
  // objects += KaseObjectPreMemberPostcomp
  // objects += KaseObjectPostMemberNocomp
  // objects += KaseObjectPostMemberPrecomp
  // objects += KaseObjectPostMemberPostcomp
  // @kase object KaseObjectPostMemberNocomp
  // class KaseObjectPostMemberPrecomp
  // @kase object KaseObjectPostMemberPrecomp
  // @kase object KaseObjectPostMemberPostcomp
  // class KaseObjectPostMemberPostcomp

  // @Test
  def combo: Unit = {
    @kase object KaseObjectPreLocalNocomp
    class KaseObjectPreLocalPrecomp
    @kase object KaseObjectPreLocalPrecomp
    @kase object KaseObjectPreLocalPostcomp
    class KaseObjectPreLocalPostcomp
    objects += KaseObjectPreLocalNocomp
    objects += KaseObjectPreLocalPrecomp
    objects += KaseObjectPreLocalPostcomp
    objects += KaseObjectPostLocalNocomp
    objects += KaseObjectPostLocalPrecomp
    objects += KaseObjectPostLocalPostcomp
    @kase object KaseObjectPostLocalNocomp
    class KaseObjectPostLocalPrecomp
    @kase object KaseObjectPostLocalPrecomp
    @kase object KaseObjectPostLocalPostcomp
    class KaseObjectPostLocalPostcomp

    assertEquals(objects.mkString("\n"), """
      |KaseObjectPreToplevelNocomp
      |KaseObjectPreToplevelPrecomp
      |KaseObjectPreToplevelPostcomp
      |KaseObjectPostToplevelNocomp
      |KaseObjectPostToplevelPrecomp
      |KaseObjectPostToplevelPostcomp
      |KaseObjectPreLocalNocomp
      |KaseObjectPreLocalPrecomp
      |KaseObjectPreLocalPostcomp
      |KaseObjectPostLocalNocomp
      |KaseObjectPostLocalPrecomp
      |KaseObjectPostLocalPostcomp
    """.trim.stripMargin)
  }
}

@kase object KaseObjectPostToplevelNocomp
class KaseObjectPostToplevelPrecomp
@kase object KaseObjectPostToplevelPrecomp
@kase object KaseObjectPostToplevelPostcomp
class KaseObjectPostToplevelPostcomp
