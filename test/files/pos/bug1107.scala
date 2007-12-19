object F {
  type AnyClass = Class[_]
  def tryf[T](ignore: List[AnyClass])(f: => T): Any = {
    try {
      f
    } catch {
      case e if ignore == null || ignore.isEmpty => {false}
    }
  }
}
