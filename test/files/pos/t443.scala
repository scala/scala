object Test {

  def lookup(): Option[Pair[String, String]] =
    ((null: Option[Pair[String, String]]) : @unchecked) match {
      case Some(Pair(_, _)) =>
	if (true)
	  Some(Pair(null, null))
	else
	  lookup() match {
	    case Some(_) => Some(null)
	    case None => None
	  }
    }
}
