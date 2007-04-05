import java.io.{OutputStream, PrintStream};

class PromptStream(s: OutputStream) extends PrintStream(s) {
    override def println() = super.println();
}

object Main {

    val out = new PromptStream(java.lang.System.out);

    java.lang.System.setOut(out);

    def main(args: Array[String]) =
        //out.println("hello world");
      ()

}
