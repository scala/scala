object Ex extends Exception
object Test {
  def ex: Any = throw Ex
  def main(args: Array[String]): Unit = {
    try {
      { ex; () }.##
      sys.error("no exception was thrown")
    } catch {
      case `Ex` =>
    }
  }
}