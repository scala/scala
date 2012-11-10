sealed trait ColumnOption
case object NotNull extends ColumnOption
case object PrimaryKey extends ColumnOption
case object lower extends ColumnOption

object Test {
  def main(args: Array[String]) {
    val l = List(PrimaryKey, NotNull, lower)
    for (option @ NotNull <- l) println("Found " + option)
    for (option @ `lower` <- l) println("Found " + option)
    for ((`lower`, i) <- l.zipWithIndex) println("Found " + i)
  }
}
