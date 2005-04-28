package scala.util.parsing;

class SimpleTokenizer(in: Iterator[char], delimiters: String) extends Iterator[String] {

  val tracing = false;

  private def max(x: int, y: char): int = if (x < y) y else x;

  private def delimArray: Array[boolean] = {
    val ds = List.fromString(delimiters);
    val da = new Array[boolean]((0 /: ds)(max) + 1);
    for (val ch <- ds) { da(ch) = true }
    da
  }

  private val isdelim = delimArray;
  private def isDelimiter(ch: int) = ch >= 0 && ch < isdelim.length && isdelim(ch);

  private val EOI = -1;

  private def nextChar: int = if (in.hasNext) in.next else EOI;

  private var ch: int = nextChar;

  private val buf = new StringBuffer();

  def hasNext: boolean = ch != EOI;

  def next: String = {
    while (ch <= ' ' && ch != EOI) ch = nextChar;
    if (ch == EOI) ""
    else {
      buf.setLength(0);
      if (isDelimiter(ch)) {
        buf append ch.asInstanceOf[char]; ch = nextChar
      } else {
	while (ch > ' ' && ch != EOI && !isDelimiter(ch)) {
          buf append ch.asInstanceOf[char]; ch = nextChar;
        }
      }
      if (tracing) System.out.println("<" + buf.toString() + ">");
      buf.toString()
    }
  }
}

