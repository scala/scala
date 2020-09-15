package tastytest

object TestTermRefIns extends Suite("TestTermRefIns") {

  def extractConfig(config: GenericConfig): String = config match {
    case embedded: EmbeddedConfig => embedded.config
    case x                        => throw new MatchError(x)
  }

  test(assert(extractConfig(Configurations.defaultConfig) === "config"))
}
