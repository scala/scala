//> using options -Werror
sealed trait PathAndQuery
sealed trait Path  extends PathAndQuery
sealed trait Query extends PathAndQuery

object PathAndQuery {
  case object Root                        extends Path
  case class /(prev: Path, value: String) extends Path

  case class ===(k: String, v: String)    extends Query
  case class :&(prev: Query, next: (===)) extends Query
  case class +?(path: Path,  next: (===)) extends Query
}

object Main {
  def main(args: Array[String]): Unit = {
    import PathAndQuery._

    val path = /(/(Root, "page"), "1")
    val q1   = ===("k1", "v1")
    val q2   = ===("k2", "v2")
    val pq   = :&(+?(path, q1), q2)

    (pq: PathAndQuery) match {
      case Root / "page" / "1"                                       => println("match 1")
      case Root / "page" / "1" +? ("k1" === "v1")                    => println("match 2")
      case Root / "page" / "1" +? ("k1" === "v1") :& ("k2" === "v2") => println("match 3")
    }
  }
}
