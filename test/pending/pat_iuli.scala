trait Ops requires MyCodes {
  abstract class Instru
  object opcodes {
    case class SWITCH(i:Int) extends Instru
  }
}

trait Blox requires MyCodes {
  import opcodes._
  class Basick {
    var foo: Instru = null

    def bar = foo match {
      case SWITCH(i) => i
    }
  }
}

abstract class MyCodes extends AnyRef with Ops with Blox {
}
