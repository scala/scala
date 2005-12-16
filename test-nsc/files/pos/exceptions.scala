import java.io._;

object Test {

  //def error[a](x: String):a = new java.lang.RuntimeException(x) throw;

  def main(args: Array[String]): Unit = {
    try {
      try {
	System.out.println("hi!");
        error("xx");
      } finally {
        System.out.println("ho!")
      }
    } catch {
      case ex: IOException => System.out.println("io exception!");
      case ex => System.out.println(ex);
    }
  }
}
