object Exceptions {

    class CubeException(s: String) extends java.lang.RuntimeException(s);

    def main(args: Array[String]) =
        System.out.println(new CubeException("test"));

}
