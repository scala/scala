trait Driver {
  abstract class Table
}

object Config {
  val driver : Driver = ???
  def driver(a: Any) = ???
}

object Sites extends Config.driver.Table
