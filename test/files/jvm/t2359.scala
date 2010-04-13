import scala.actors.Futures._

object Test {
  def main(args: Array[String]) {
    val x = future {
      try {
      System.out.println(1)
      future {
        try {
        System.out.println(2)
        future {
          try {
          System.out.println(3)
          future {
            try {
            System.out.println(4)
            future {
              try {
              System.out.println(5)
              } catch {
                case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
                  e.printStackTrace()
              }
            }()
            } catch {
              case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
                e.printStackTrace()
            }
          }()
          } catch {
            case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
              e.printStackTrace()
          }
        }()
        } catch {
          case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
            e.printStackTrace()
        }
      }()
      } catch {
        case e: Throwable if !e.isInstanceOf[scala.util.control.ControlThrowable] =>
          e.printStackTrace()
      }
    }()
  }
}
