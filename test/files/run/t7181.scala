sealed abstract class Action
// exit the try body normally
case object MainNormalExit extends Action
// exit the try body with a 'return'
case object MainReturn extends Action
// exit the try body with an uncaught exception
case object MainUncaughtException extends Action
// exit the try body with a caught exception and exit the exception handler normally
case object ExceptionNormalExit extends Action
// exit the try body with a caught exception and exit the exception handler with a 'return'
case object ExceptionReturn extends Action
// exit the try body with a caught exception and exit the exception handler with an uncaught exception
case object ExceptionUncaughtException extends Action

case class UncaughtException(action: Action) extends RuntimeException
case class CaughtException(action: Action) extends RuntimeException

object Test extends App {
  def test(action: Action, expectException: Boolean = false) {
    var gotException = false
    val result = try
      driver(action)
    catch {
      case UncaughtException(a) =>
        gotException = true
        a
    }
    if (gotException) assert(expectException, "Got unexpected exception")
    else assert(!expectException, "Did not get expected exception")

    assert(result == action, s"Expected $action but got $result")
    println()
  }

  def driver(action: Action): Action = {
    val result = try {
      action match {
        case MainNormalExit =>
          println(s"normal exit $action")
          action
        case MainReturn =>
          println(s"return $action")
          return action
        case MainUncaughtException =>
          println(s"uncaught exception $action")
          throw UncaughtException(action)
        case _ =>
          println(s"caught exception $action")
          throw CaughtException(action)
      }
    } catch {
      case CaughtException(action) => action match {
        case ExceptionNormalExit =>
          println(s"normal exit $action")
          action
        case ExceptionReturn =>
          println(s"return $action")
          return action
        case ExceptionUncaughtException =>
          println(s"uncaught exception $action")
          throw UncaughtException(action)
        case _ =>
          sys.error(s"unexpected $action in exception handler")
      }
    } finally {
      println(s"finally $action")
    }
    println(s"normal flow $action")
    result
  }

  test(MainNormalExit)
  test(MainReturn)
  test(MainUncaughtException, true)
  test(ExceptionNormalExit)
  test(ExceptionReturn)
  test(ExceptionUncaughtException, true)
}
