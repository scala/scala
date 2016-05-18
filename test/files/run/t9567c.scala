case class CaseSequenceTopLevel(as: Int*)

object Test {
   def main(args: Array[String]): Unit = {

    val buffer1 = collection.mutable.Buffer(0, 0)
    CaseSequenceTopLevel(buffer1: _*) match {
      case CaseSequenceTopLevel(_, i) =>
        buffer1(1) = 1
        assert(i == 0, i) // fails in 2.11.7 -optimize
    }

    case class CaseSequence(as: Int*)
    val buffer2 = collection.mutable.Buffer(0, 0)
    CaseSequence(buffer2: _*) match {
      case CaseSequence(_, i) =>
        buffer2(1) = 1
        assert(i == 0, i)
    }

    case class CaseSequenceWithVar(var x: Any, as: Int*)
    val buffer3 = collection.mutable.Buffer(0, 0)
    CaseSequenceWithVar("", buffer3: _*) match {
      case CaseSequenceWithVar(_, _, i) => // crashes in 2.11.7
        buffer2(1) = 1
        assert(i == 0, i)
    }
  }
}
