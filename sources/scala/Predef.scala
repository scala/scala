package scala;

object Predef {

  type byte = scala.Byte;
  type short = scala.Short;
  type char = scala.Char;
  type int = scala.Int;
  type long = scala.Long;
  type float = scala.Float;
  type double = scala.Double;
  type boolean = scala.Boolean;
  type unit = scala.Unit;

  def List[a](x: a*): List[a] = x as List[a];
  val List = scala.List;

  def Array[a <: AnyRef](classname: String)(x: a*): Array[a] = {
    System.out.println("array1");
    val xs0: Array[java.lang.Object] = runtime.RunTime.oarray(x.length, classname);
    val xs: Array[a] = xs0 as Array[a];
    System.out.println("array2");
    val elems = x.elements;
    var i = 0;
    while (elems.hasNext) { xs(i) = elems.next; System.out.println(xs(i)) }
    xs
  }

  def error(x: String): All = new java.lang.RuntimeException(x).throw;

  def try[a](def block: a): Except[a] =
    new Except(scala.runtime.ResultOrException.tryBlock(block));

  def while(def condition: Boolean)(def command: Unit): Unit =
    if (condition) {
      command; while(condition)(command)
    } else {
    }

  trait Until {
    def until(def condition: Boolean): Unit
  }

  def repeat(def command: Unit): Until =
    new Until {
      def until(def condition: Boolean): Unit = {
	command ;
	if (condition) {}
	else until(condition)
      }
    }

  type Pair = Tuple2;
  def Pair[a, b](x: a, y: b) = Tuple2(x, y);

  type Triple = Tuple3;
  def Triple[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z);
}





