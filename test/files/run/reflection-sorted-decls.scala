object Test  {
  def main(args: Array[String]) {
    class Foo(val a: Int, val b: Int, val c: Int)
    import scala.reflect.runtime.{currentMirror => cm}
    val decls = cm.classSymbol(classOf[Foo]).info.decls
    decls.sorted.toList.filter(!_.isMethod) foreach System.out.println
  }
}
