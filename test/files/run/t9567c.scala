case class CaseSequenceTopLevel(as: Int*)

object Test {
  def main(args: Array[String]): Unit = {
    val a1 = Array(0, 0)
    val v1 = collection.immutable.ArraySeq.unsafeWrapArray(a1)
    CaseSequenceTopLevel(v1: _*) match {
      case CaseSequenceTopLevel(_, i) =>
        a1(1) = 1
        assert(i == 0, i) // fails in 2.11.7 -optimize
    }

    case class CaseSequence(as: Int*)
    val a2 = Array(0, 0)
    val v2 = collection.immutable.ArraySeq.unsafeWrapArray(a2)
    CaseSequence(a2: _*) match {
      case CaseSequence(_, i) =>
        a2(1) = 1
        assert(i == 0, i)
    }

    case class CaseSequenceWithVar(var x: Any, as: Int*)
    val a3 = Array(0, 0)
    val v3 = collection.immutable.ArraySeq.unsafeWrapArray(a3)
    CaseSequenceWithVar("", a3: _*) match {
      case CaseSequenceWithVar(_, _, i) => // crashes in 2.11.7
        a3(1) = 1
        assert(i == 0, i)
    }
  }
}
