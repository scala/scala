import java.io.IOException;

object Test {

  //def error[a](x: String):a = new java.lang.RuntimeException(x) throw;

  def main(args: Array[String]): Unit = {
    try {
      try {
	Console.println("hi!");
        error("xx");
      } finally {
        Console.println("ho!")
      }
    } catch {
      case ex: IOException => Console.println("io exception!");
      case ex => Console.println(ex);
    }
  }
}
