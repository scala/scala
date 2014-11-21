trait Driver {
  abstract class Table
}

object Config {
  val driver : Driver = ???
  val driverUniqueName: driver.type = driver
  def driver(a: Any) = ???
}

object Sites extends Config.driver.Table
