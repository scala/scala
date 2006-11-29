package test;

object Main {
  def main(args : Array[String]) : Unit = {
    new ExtC {
      type A = Ax;
      class Ax extends super.Ax;
    }
  }
}
