package plugintemplate

import org.scalatest.Suite

class PluginPropertiesSuite extends Suite {
  def testProperties() {
    expect("A template compiler plugin saying hello to the World") {
      PluginProperties.pluginDescription
    }
    expect("0.1") {
      PluginProperties.versionString
    }
  }
}
