class Crashy {
  def g(): Option[Any] = None
  
  def crashy() = {
    for (_ <- g()) {
      (null: Any) match {
        case Some(_) => 5
        case None    => sys.error("")
      }
    }
  }
}

