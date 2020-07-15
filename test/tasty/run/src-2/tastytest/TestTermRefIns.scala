package tastytest

object TestTermRefIns extends Suite("TestTermRefIns") {

  def extractConfig(config: GenericConfig): String = config match {
    case embedded: EmbeddedConfig => embedded.config
  }

  test(assert(extractConfig(Configurations.defaultConfig) === "config"))
}
