object Test  {
  def main(args: Array[String]) {
    trait T1 { def a: Int; def c: Int }
    trait T2 { def a: Int; def b: Int }
    class Bar(val x: Int)
    class Foo(val a: Int, val b: Int, val c: Int) extends Bar(a + b + c) with T1 with T2
    import scala.reflect.runtime.{currentMirror => cm}
    val members = cm.classSymbol(classOf[Foo]).info.members
    members.sorted.toList.filter(!_.isMethod) foreach System.out.println
  }
}
