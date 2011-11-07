trait Linked {
  type File <: FileImpl;
  trait FileImpl {
    Console.println("Hello from linked");
  }
}
object Test {
  class Test extends Linked { 
    trait FileImpl extends super.FileImpl {
//      val x: int = 1
    }
    class File extends FileImpl;
  }
  def main(args : Array[String]) : Unit = {
    Console.println("BEGIN");
    val test = new Test;
    val file = new test.File;
    Console.println("END");
  }
}
