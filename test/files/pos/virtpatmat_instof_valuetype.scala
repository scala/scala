case class Data(private val t: Option[String] = None, only: Boolean = false) {
  def add(other: Data) = {
    other match {
      case Data(None, b)    => ()
      case Data(Some(_), b) => ()
    }
  }
}