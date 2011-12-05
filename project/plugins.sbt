resolvers += Resolver.url("Typesafe nightlies", url("https://typesafe.artifactoryonline.com/typesafe/ivy-snapshots/"))(Resolver.ivyStylePatterns)

resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.6"


