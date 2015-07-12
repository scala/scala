package scales.xml
//import scales.xml._ // using another package and importing doesn't CCE

object CCE_Test {
  def main(args: Array[String]): Unit = {
    // without the import it doesn't trigger the CCE
    import scaley.funny._

    val pull = null.asInstanceOf[Iterator[PullType]]
    val LogEntries = null.asInstanceOf[List[QName]]
    // fully qualify with scales.xml. and it won't trigger it
    iterate(LogEntries,
      pull)
  }
}
