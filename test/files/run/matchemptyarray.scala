import scala.tools.partest.Util.ArrayDeep

object Test extends App{
  Array[String]() match {
    case x@Array() => println(x.deep.toString());
  }
}
