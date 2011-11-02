import sbt._
trait BuildInfoEnvironment {
  self : Project =>
  def buildInfoEnvironmentLocation: Path
  /**
   * Environment for storing properties that
   * 1) need to be saved across sbt session
   * 2) Are local to a layer
   * Used to save the last version of the compiler used to build the layer (for discarding it's product if necessary)
   */
  lazy val layerEnvironment = new BasicEnvironment {
    // use the project's Logger for any properties-related logging
    def log = self.log

    // the properties file will be read/stored 
    def envBackingPath = buildInfoEnvironmentLocation
    // define some properties
    lazy val lastCompilerVersion: Property[String] = propertyOptional[String]("")
  } 

}
