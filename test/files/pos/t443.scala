object Test {

  def lookup(): Option[Tuple2[String, String]] =
    ((null: Option[Tuple2[String, String]]) : @unchecked) match {
      case Some((_, _)) =>
	if (true)
	  Some((null, null))
	else
	  lookup() match {
	    case Some(_) => Some(null)
	    case None => None
	  }
    }
}
