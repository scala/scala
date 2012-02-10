class Test {
  type AnyCyclic = Execute[Task]#CyclicException[_]

  trait Task[T]

  trait Execute[A[_] <: AnyRef] {
    class CyclicException[T](val caller: A[T], val target: A[T])
  }

  def convertCyclic(c: AnyCyclic): String =
    (c.caller, c.target) match {
      case (caller: Task[_], target: Task[_]) => "bazinga!"
    }
}

