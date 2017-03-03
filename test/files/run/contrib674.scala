// causes VerifyError with scala-2.5.1

object Test extends App {
  def bad(): Unit = {
    try {
      1
    } catch {
      case e: Throwable =>
    } finally {
      try {
      } catch {
        case e: Throwable =>
      }
    }
    1
  }

  bad
}
