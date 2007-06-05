trait Ops { self: MyCodes =>
  abstract class Instru
  object opcodes {
    case class SWITCH(i:Int) extends Instru
    case object EmptyInstr extends Instru
  }
}

trait Blox { self: MyCodes =>
  import opcodes._
  class Basick {
    var foo: Instru = null

    def bar = foo match {
      case SWITCH(i) => i
      case EmptyInstr => 0
    }
  }
}

abstract class MyCodes extends AnyRef with Ops with Blox {
}
