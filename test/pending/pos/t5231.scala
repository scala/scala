object Client {
   sealed trait ConfigLike {
      def clientID:     Int
   }

   object Config {
      def apply() : ConfigBuilder = new ConfigBuilder()
      implicit def build( cb: ConfigBuilder ) : Config = cb.build
   }

   final class Config private[Client]( val clientID: Int )
   extends ConfigLike

   final class ConfigBuilder private () extends ConfigLike {
      var clientID: Int = 0
      def build : Config = new Config( clientID )
   }
}
