trait H2Driver{
    abstract class Table[T]
}

object Config {
  val driver : H2Driver = ???
  def driver(app: Any): H2Driver = ???
}

class Sites extends Config.driver.Table[String]
