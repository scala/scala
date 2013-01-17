trait LogLevelType
object Test {
  type LogLevel = Int with LogLevelType
  final val ErrorLevel = 1.asInstanceOf[Int with LogLevelType]
  def main(args: Array[String]) {
    List(ErrorLevel, ErrorLevel)
  }
}
