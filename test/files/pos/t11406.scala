
class Var[T](private var value: T) {
  def get: T = value
  def update(newValue: T) = value = newValue
}

object Test extends App {
  type UpdatePair[T] = (Var[T], T => T)

  def doUpdates[A](updateChain: List[UpdatePair[A]]) = {
    updateChain.groupBy(_._1) foreach {
      case (v: Var[t], ops) =>
        var current: t = v.get
        // This is what I want to get rid of:
        type Op = t => t
        ops foreach {
          case (_, op: Op) => current = op(current)
        }
        v() = current
    }
  }
  def doUpdates2[A](updateChain: List[UpdatePair[A]]) = {
    updateChain.groupBy(_._1) foreach {
      case (v: Var[t], ops) =>
        var current: t = v.get
        ops foreach {
          case (_, op: Function1[`t`, `t`]) => current = op(current)
        }
        v() = current
    }
  }
}

