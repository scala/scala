object F {
  def tryf[T](ignore: List[Class])(f: => T): Any = {
    try {
      f
    } catch {
      case e if ignore == null || ignore.isEmpty => {false}
    }
  }
}
