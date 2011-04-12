import scala.tools.nsc._

object Test {
  val x = {
      val settings = new Settings()
      settings.classpath.value = System.getProperty("java.class.path")

    object cc extends Global(settings) {
      object dummy

      override def computePluginPhases() = {
        super.computePluginPhases()
        assert(dummy ne null, "Dummy not initialized")
      }
    }
    new cc.Run
    ()
  }

  def main(args: Array[String]): Unit = {

  }
}

