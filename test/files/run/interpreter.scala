import scala.tools.nsc._
import java.io.{BufferedReader, StringReader, PrintWriter,
                Writer, OutputStreamWriter}

object Test {
  val testCodeString = <code>
// basics
3+4
def gcd(x: int, y: int): int = {{
          if (x == 0) y
          else if (y == 0) x
          else gcd(y%x, x)
}}
val five = gcd(15,35)
var x = 1
x = 2
val three = x+1
type anotherint = Int
val four: anotherint = 4
val bogus: anotherint = "hello"
trait PointlessTrait

// implicit conversions
case class Foo(n: int)
case class Bar(n: int)
implicit def foo2bar(foo: Foo) = Bar(foo.n)
val bar: Bar = Foo(3)

// importing from a previous result
import bar._
val m = n

// stressing the imports mechanism
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1
val one = 1


val x1 = 1
val x2 = 1
val x3 = 1
val x4 = 1
val x5 = 1
val x6 = 1
val x7 = 1
val x8 = 1
val x9 = 1
val x10 = 1
val x11 = 1
val x12 = 1
val x13 = 1
val x14 = 1
val x15 = 1
val x16 = 1
val x17 = 1
val x18 = 1
val x19 = 1
val x20 = 1

val two = one + x5


// interior syntax errors should *not* go into multi-line input mode.
// both of the following should abort immediately:
def x => y => z
[1,2,3]


// multi-line XML
&lt;a>
&lt;b
  c="c"
  d="dd"
/>&lt;/a>

(1 +   // give up early by typing two blank lines




</code>.text


  /** A writer that skips the first line of text.  The first
   *  line of interpreter output is skipped because it includes
   *  a version number. */
  class Skip1Writer(writer: Writer) extends Writer {
    var seenNL = false

    def write(cbuf: Array[Char], off: Int, len: Int) {
      if (seenNL)
	writer.write(cbuf, off, len)
      else {
	val slice = cbuf.slice(off, off+len)
	val i = slice.indexOf('\n')
	if (i >= 0) {
	  seenNL = true
	  writer.write(slice, i+1, slice.length-(i+1))
	} else {
	  // skip it
	}
      }
    }

    def close() { writer.close() }
    def flush() { writer.flush() }
  }


  def main(args: Array[String]) {
    val input = new BufferedReader(new StringReader(testCodeString))
    val output = new PrintWriter(
      new Skip1Writer(new OutputStreamWriter(Console.out)))
    val repl = new InterpreterLoop(input, output)
    repl.main(new Settings)
    println()
  }
}
