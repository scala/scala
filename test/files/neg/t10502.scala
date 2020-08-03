object Bug {
  object Perhaps {
    def unapply[A](oa: Option[A]): Some[Option[A]] = Some(oa)
  }

  def single(opt: Option[String]) = opt match {
    case Perhaps(Some(s)) => s
  }

  def list(list: List[Option[String]]) = list match {
    case Perhaps(Some(s)) :: _ => s
    case Perhaps(None   ) :: _ => "<none>"
    case Nil                   => "<nil>"
  }
}
