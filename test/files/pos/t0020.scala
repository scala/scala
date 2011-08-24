object Exceptions {

    class CubeException(s: String) extends RuntimeException(s);

    def main(args: Array[String]) =
        Console.println(new CubeException("test"));

}
