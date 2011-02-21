// causes VerifyError with scala-2.5.1

object Test extends App {
  def bad() {
    try {
      1
    } catch {
      case e =>
    } finally {
      try {
      } catch {
        case e =>
      }
    }
    1
  }

  bad
}
