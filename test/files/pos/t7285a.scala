sealed abstract class Base

object Test {
  case object Up extends Base

  def foo(d1: Base) =
    d1 match {
      case Up  =>
    }

    // Sealed subtype: ModuleTypeRef   <empty>.this.Test.Up.type
    // Pattern:        UniqueThisType  Test.this.type
}


object Test1 {
  sealed abstract class Base

  object Base {
    case object Down extends Base {
    }

    case object Up extends Base {
    }

    (d1: Base, d2: Base) =>
      (d1, d2) match {
        case (Up, Up) | (Down, Down) => false
        case (Down, Up)              => true
        case (Up, Down)              => false
      }
  }
}

object Test2 {
  sealed abstract class Base

  object Base {
    case object Down extends Base {
    }

    case object Up extends Base {
    }

    (d1: Base, d2: Base) =>
      (d1) match {
        case Up | Down => false
      }
  }
}

object Test3 {
  sealed abstract class Base

  object Base {
    case object Down extends Base

    (d1: Base, d2: Base) =>
      (d1, d2) match {
        case (Down, Down) => false
      }
  }
}

object Test4 {
  sealed abstract class Base

  object Base {
    case object Down extends Base {
    }

    case object Up extends Base {
    }

  }
  import Test4.Base._
  (d1: Base, d2: Base) =>
    (d1, d2) match {
      case (Up, Up) | (Down, Down) => false
      case (Down, Test4.Base.Up)   => true
      case (Up, Down)              => false
    }
}
