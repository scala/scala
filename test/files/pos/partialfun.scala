object partialfun {

  def applyPartial[b](f: PartialFunction[Option[String], b])(x: Option[String]) =
    if (f.isDefinedAt(x)) f(x) else "<undefined>";

  applyPartial {
    case Some(xxx) => xxx
    case None => throw new MatchError(None)
  } (None);

  // Again, but using function literal
  applyPartial(_.get)(None)

  // Now test this case involving an implicit conversion and auto-tupling
  // derived from AbstractFSM's onTransition; akka/akka#27410
  implicit final def g(f: (String, String) => Unit): PartialFunction[(String, String), Unit] = ???
  val fun: (String, String) => Unit               = ???
  val pf: PartialFunction[(String, String), Unit] = fun(_: String, _: String)
}
