import java.io._;

class PromptStream(s: OutputStream) extends PrintStream(s) {
    override def println() = super.println();
}

object Main {

    val out = new PromptStream(System.out);

    System.setOut(out);

    def main(args: Array[String]) =
        //out.println("hello world");
      ()

}
