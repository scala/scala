import java.io._;

object test {

  //def error[a](x: String):a = new java.lang.RuntimeException(x) throw;

  def main(): Unit = {
    try {
      try {
	System.out.println("hi!");
        error("xx");
      } finally {
        System.out.println("ho!")
      }
    } except {
      case ex: IOException => System.out.println("io exception!");
      case ex => System.out.println(ex);
    }
  }

  main();
}
