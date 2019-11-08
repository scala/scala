package scala.tools.tastytest

sealed abstract class SourceKind(val name: String)(val filter: String => Boolean = _.endsWith(name))

object SourceKind {

  case object NoSource  extends SourceKind("")(filter = _ => false)
  case object Scala     extends SourceKind(".scala")()
  case object ScalaFail extends SourceKind("_fail.scala")()
  case object Check     extends SourceKind(".check")()

  def whitelist(kinds: Set[SourceKind], paths: String*): Seq[String] =
    if (kinds.isEmpty) Nil
    else paths.filter(kinds.foldLeft(NoSource.filter)((filter, kind) => p => kind.filter(p) || filter(p)))
}
