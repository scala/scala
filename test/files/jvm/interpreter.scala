import scala.tools.nsc._
import java.io.{BufferedReader, StringReader, PrintWriter,
                Writer, OutputStreamWriter}

object Test {
  val testCodeString = <code>
// basics
3+4
def gcd(x: Int, y: Int): Int = {{
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
val (x,y) = (2,3)
println("hello")

// ticket #1513
val t1513 = Array(null)
// ambiguous toString problem from #547
val atom = new scala.xml.Atom()
// overriding toString problem from #1404
class S(override val toString : String)
val fish = new S("fish")
// Test that arrays pretty print nicely.
val arr = Array("What's", "up", "doc?") 
// Test that arrays pretty print nicely, even when we give them type Any
val arrInt : Any = Array(1,2,3)
// Test that nested arrays are pretty-printed correctly
val arrArrInt : Any = Array(Array(1, 2), Array(3, 4))

// implicit conversions
case class Foo(n: Int)
case class Bar(n: Int)
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

// handling generic wildcard arrays (#2386)
// It's put here because type feedback is an important part of it.
val xs: Array[_] = Array(1, 2)
xs.size
xs.head
xs filter (_ == 2)
xs map (_ => "abc")
xs map (x => x)
xs map (x => (x, x))

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


/*
  /*
    multi-line comment
  */
*/


// multi-line string
"""
hello
there
"""

(1 +   // give up early by typing two blank lines


// defining and using quoted names should work (ticket #323)
def `match` = 1 
val x = `match` 

// multiple classes defined on one line
sealed class Exp; class Fact extends Exp; class Term extends Exp
def f(e: Exp) = e match {{  // non-exhaustive warning here
  case _:Fact => 3
}}

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
	val slice : Array[Char] = cbuf.slice(off, off+len)
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
    val settings = new Settings
    // when running that compiler, give it a scala-library to the classpath
    settings.classpath.value = System.getProperty("java.class.path")
    repl.main(settings)
    println()

    val interp = new Interpreter(settings)
    interp.interpret("def plusOne(x: Int) = x + 1")
    interp.interpret("plusOne(5)")
    interp.reset()
    interp.interpret("\"after reset\"")
    interp.interpret("plusOne(5) // should be undefined now")
  }
}
