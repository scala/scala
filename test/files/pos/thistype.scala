object Test {

  class Ctl {
    def enable: this.type = { Console.println("enable"); this }
  }

  class MouseCtl extends Ctl {
    def mouseDown(x: int, y: int): unit = { Console.println("mouse down"); }
  }

  def main(args: Array[String]) =
    new MouseCtl().enable.mouseDown(1, 2);

}
