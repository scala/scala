import reflect.runtime.universe._

object Test {
  def main(args: Array[String]) {
    for (clsName <- List("a.b1.c", "a.b2.c")) {
      println(rootMirror.classSymbol(Class.forName("a.b1.c")))
      println(rootMirror.classSymbol(Class.forName("a.b2.c")))
    }
  }
}
