// TODO: are the resolvers needed?
resolvers ++= Seq(Resolver.typesafeIvyRepo("releases"), Resolver.sbtPluginRepo("releases"))

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")
