package scala.swing

abstract class SimpleSwingApplication extends SwingApplication {
  def top: Frame

  override def startup(args: Array[String]) {
    val t = top
    t.pack()
    t.visible = true
  }

  def resourceFromClassloader(path: String): java.net.URL =
    this.getClass.getResource(path)

  def resourceFromUserDirectory(path: String): java.io.File =
    new java.io.File(System.getProperty("user.dir"), path)
}
