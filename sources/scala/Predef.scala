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

    def List[A](x: A*): List[A] = x as List[A];
    val List = scala.List;

    def Set[A](es: A*): Set[A] = {
        val set = new HashSet[A];
        set.addSet(es);
        set;
    }

    def ListSet[A](es: A*): Set[A] = {
        val set = new ListSet[A];
        set.addSet(es);
        set;
    }

    def HashSet[A](es: A*): Set[A] = {
        val set = new HashSet[A];
        set.addSet(es);
        set;
    }

    def Map[A, B](mappings: Pair[A, B]*): Map[A, B] = {
        val map = new HashMap[A, B];
        map.putMap(mappings);
        map;
    }

    def error(x: String): All = new java.lang.RuntimeException(x).throw;

    def exit: scala.Unit = System.exit(0);

    def synchronized[A](obj: AnyRef)(def body: A) = NativeMonitor.synchronised(obj, body);

    def try[a](def block: a): Except[a] =
        new Except(scala.runtime.ResultOrException.tryBlock(block));

    def while(def cond: Boolean)(def body: Unit): Unit = NativeLoop.loopWhile(cond, body);
        /* if (cond) {
            body; while(cond)(body)
        } */

    trait Until {
        def until(def condition: Boolean): Unit
    }

    def repeat(def command: Unit): Until =
        new Until {
            def until(def condition: Boolean): Unit = {
                command ;
                if (condition) {} else until(condition)
            }
        }

    type Pair = Tuple2;
    def Pair[a, b](x: a, y: b) = Tuple2(x, y);

    type Triple = Tuple3;
    def Triple[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z);
}
