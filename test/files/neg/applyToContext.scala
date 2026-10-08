class Config
class Logger
class Service(config: Config, logger: Logger)

object Test {
  implicit val config: Config = new Config

  def service: Service = ((c: Config, l: Logger) => new Service(c, l)).applyToContext // error: no implicit Logger
}
