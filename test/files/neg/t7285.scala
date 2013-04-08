sealed abstract class Base


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
        case Test2.Base.Up => false
      }
  }
}


object Test4 {
  sealed abstract class Base

  object Base {
    case object Down extends Base

    case object Up extends Base
  }

  import Test4.Base._
  (d1: Base, d2: Base) =>
    (d1, d2) match {
      case (Up, Up) | (Down, Down) => false
      case (Down, Test4.Base.Up)   => true
    }
}
