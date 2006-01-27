object Test {
  def main(args:Array[String]): Unit = {
    try {
      throw new Error();
    }
    catch {
      case _ => Console.println("exception happened\n");
    }
  }
}
