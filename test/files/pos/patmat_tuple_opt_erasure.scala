object T {
  def layoutFromParent(x: Option[String], y: Option[String]): (String, String) = (x, y) match {
    case (_,          None  )  => ???
    case (None,       Some(_)) => ???
    case (None,       Some(_)) => ???
    case (Some(_), Some(_))    => ???
    case (Some(_), Some(_))    => ???
  }
}