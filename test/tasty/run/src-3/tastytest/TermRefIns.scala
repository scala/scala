package tastytest

trait GenericConfig

sealed trait EmbeddedConfig extends GenericConfig { // scala.annotation.internal.Child annotation with TERMREFin argument
  val config: String
}

trait DefaultConfigs {

  val defaultConfig: EmbeddedConfig = PrivateConfig

  private case object PrivateConfig extends EmbeddedConfig {
    val config: String = "config"
  }

}
object Configurations extends DefaultConfigs
