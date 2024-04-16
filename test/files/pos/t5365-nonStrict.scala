//> using options -Werror -Xnon-strict-patmat-analysis
//
// copy of neg/t5365.scala, which under -Xnon-strict-patmat-analysis gives no warnings
class C {
  def nonExhautiveIfWeAssumeGuardsTrueOrFalse(x: Option[Int]): Int = x match {
    case Some(n) if n % 2 == 0 => n
  }

  def nonExhautiveIfWeAssumeGuardsFalse(x: Option[Int]): Int = x match {
    case Some(n) if n % 2 == 0 => n
    case None => 0
  }

  def inverseGuards(x: Option[Int]): Int = x match {
    case Some(n) if n > 0 => n
    case Some(n) if n <= 0 => ???
    case None => 0
  }

  def extractor(x: Option[Int]) = x match {
    case Some(Extractor(_)) =>
  }
  def repeatedExtractor(x: Option[Int]) = x match {
    case Some(RepeatedExtractor(_)) =>
  }
  def extractorStrict(x: Option[Int]) = x match {
    case Some(Extractor(_)) =>
    case None =>
  }
  def repeatedExtractorStrict(x: Option[Int]) = x match {
    case Some(RepeatedExtractor(_)) =>
    case None =>
  }
}

object Extractor {
  def unapply(a: Any): Option[Any] = None
}

object RepeatedExtractor {
  def unapplySeq(a: Any): Option[Seq[Any]] = None
}
