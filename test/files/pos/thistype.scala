object Test {

  class Ctl {
    def enable: this.type = { Console.println("enable"); this }
  }

  class MouseCtl extends Ctl {
    def mouseDown(x: Int, y: Int): Unit = { Console.println("mouse down") }
  }

  def main(args: Array[String]): Unit = {
    new MouseCtl().enable.mouseDown(1, 2)
  }

}
