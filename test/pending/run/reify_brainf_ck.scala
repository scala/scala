import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    import scala.annotation._

    trait Func[T] {
        val zero: T
        def inc(t: T): T
        def dec(t: T): T
        def in: T
        def out(t: T): Unit
    }

    object ByteFunc extends Func[Byte] {
      override val zero: Byte = 0
      override def inc(t: Byte) = ((t + 1) & 0xFF).toByte
      override def dec(t: Byte) = ((t - 1) & 0xFF).toByte
      override def in: Byte = readByte
      override def out(t: Byte) { print(t.toChar) }
    }

    case class Tape[T](left: List[T], cell: T, right: List[T])(implicit func: Func[T]) {
      private def headOf(list:List[T]) = if (list.isEmpty) func.zero else list.head
      private def tailOf(list:List[T]) = if (list.isEmpty) Nil else list.tail
      def isZero = cell == func.zero
      def execute(ch: Char) = (ch: @switch) match {
       case '+' => copy(cell = func.inc(cell))
       case '-' => copy(cell = func.dec(cell))
       case '<' => Tape(tailOf(left), headOf(left), cell :: right)
       case '>' => Tape(cell :: left, headOf(right), tailOf(right))
       case '.' => func.out(cell); this
       case ',' => copy(cell = func.in)
       case '[' | ']' => this
       case _ => error("Unexpected token: " + ch)
      }
    }

    object Tape {
      def empty[T](func: Func[T]) = Tape(Nil, func.zero, Nil)(func)
    }

    class Brainfuck[T](func:Func[T]) {

      def execute(p: String) {
        val prog = p.replaceAll("[^\\+\\-\\[\\]\\.\\,\\>\\<]", "")

        @tailrec def braceMatcher(pos: Int, stack: List[Int], o2c: Map[Int, Int]): Map[Int,Int] =
          if(pos == prog.length) o2c else (prog(pos): @switch) match {
            case '[' => braceMatcher(pos + 1, pos :: stack, o2c)
            case ']' => braceMatcher(pos + 1, stack.tail, o2c + (stack.head -> pos))
            case _ => braceMatcher(pos + 1, stack, o2c)
          }

        val open2close = braceMatcher(0, Nil, Map())
        val close2open = open2close.map(_.swap)

        @tailrec def ex(pos:Int, tape:Tape[T]): Unit =
          if(pos < prog.length) ex((prog(pos): @switch) match {
              case '[' if tape.isZero => open2close(pos)
              case ']' if ! tape.isZero => close2open(pos)
              case _ => pos + 1
            }, tape.execute(prog(pos)))

        println("---running---")
        ex(0, Tape.empty(func))
        println("\n---done---")
      }
    }

    val bf = new Brainfuck(ByteFunc)
    bf.execute(""">+++++++++[<++++++++>-]<.>+++++++[<++
                  ++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]
                  <.#>+++++++++++[<+++++>-]<.>++++++++[<++
                  +>-]<.+++.------.--------.[-]>++++++++[<++++>
                  -]<+.[-]++++++++++.""")
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
