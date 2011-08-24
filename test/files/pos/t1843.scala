/**
* Scala Compiler Will Crash On this File
* ... Or Will It?
*
*/

object Crash {
  trait UpdateType[A]
  case class StateUpdate[A](updateType : UpdateType[A], value : A)
  case object IntegerUpdateType extends UpdateType[Integer]

  //However this method will cause a crash
  def crash(updates: List[StateUpdate[_]]) {
    updates match {
      case Nil =>
      case u::us =>
        u match {
          //Line below seems to be the crashing line
          case StateUpdate(key, newValue) if (key == IntegerUpdateType) =>
            println("Requires a statement to induce the crash")
          case _ =>
        }
    }
  }
}
