object partialfun {

  def applyPartial[b](f: PartialFunction[Option[String], b])(x: Option[String]) = 
    if (f.isDefinedAt(x)) f(x) else "<undefined>";

  applyPartial {
    case Some(xxx) => xxx
    case None => throw new MatchError(None)
  } (None);

}
