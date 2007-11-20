import javax.microedition.lcdui._
import javax.microedition.midlet.MIDlet

class Test extends MIDlet with CommandListener {

  def startApp {
    val display = Display.getDisplay(this)

    val mainForm = new Form("randoms")
    mainForm append "Welcome to the world of MIDlets!"
    mainForm append "(build with Scala)"
    val rnd = new Random
    for (i <- 0 until 10) mainForm append rnd.nextInt.toString

    val exitCommand = new Command("Exit", Command.EXIT, 0)
    mainForm addCommand exitCommand
    mainForm setCommandListener this

    display setCurrent mainForm
  }
  def pauseApp {}

  def destroyApp(unconditional: Boolean) {}

  def commandAction(c: Command, s: Displayable) {
    if (c.getCommandType == Command.EXIT)
      notifyDestroyed
  }
}
