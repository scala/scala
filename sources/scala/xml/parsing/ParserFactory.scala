package scala.xml.parsing ;

/** factory object to make construcing parsers from files,arrays,iterators... */

object ParserFactory {

  def make(handle:ConstructingHandler, input:java.io.File): ConstructingParser = {
    val arr: Array[Byte] = new Array[Byte]( input.length().asInstanceOf[Int] );
    val is = new java.io.FileInputStream( input );
    is.read( arr );
    val s = new String(arr);
    make( handle, Iterator.fromString(s) );
  }

  def make(theHandle:ConstructingHandler, input:Iterator[Char]): ConstructingParser = {
    val p = new ConstructingParser() {
      val it = input;
      override val handle = theHandle;
      def nextch = { ch = it.next; pos = pos + 1; }
      def init = { ch = it.next; pos = 0; }
      override val preserveWS = true;
    };
    p.init;
    p
  }

}
