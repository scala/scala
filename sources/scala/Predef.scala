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

    def List[A](x: A*): List[A] = x.asInstanceOf[List[A]];
    val List = scala.List;

/*
    def Set[A](es: A*): scala.Set[A] = {
        val set = new HashSet[A];
        set.addSet(es);
        set;
    }

    def Map[A, B](mappings: Pair[A, B]*): MutableMap[A, B] = {
        val map = new HashMap[A, B];
        map.putMap(mappings);
        map;
    }
*/

    def error(x: String): All = throw new java.lang.RuntimeException(x);

    def exit: scala.Unit = System.exit(0);

    def id[a](x: a): a = x;

    def synchronized[A](obj: AnyRef)(def body: A) =
      scala.runtime.NativeMonitor.synchronised(obj, body);

    type Pair[p, q] = Tuple2[p, q];
    def Pair[a, b](x: a, y: b) = Tuple2(x, y);

    type Triple[a, b, c] = Tuple3[a, b, c];
    def Triple[a, b, c](x: a, y: b, z: c) = Tuple3(x, y, z);

}
