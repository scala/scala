resolvers += "Proguard plugin repo" at "http://siasia.github.com/maven2"

libraryDependencies <<= (libraryDependencies, appConfiguration) { (deps, app) =>
  deps :+ "com.github.siasia" %% "xsbt-proguard-plugin" % app.provider.id.version
}
