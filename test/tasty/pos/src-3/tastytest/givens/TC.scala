package tastytest.givens

object TCModule:
  trait TC[V]
  object TC:
    export TCInstances.TC.given

object TCInstances:
  object TC:
    import TCModule.TC
    given mkTCFromInt[V <: Int]: TC[V] with
      type Out = Int
