class C {
  val fun = () => {
    class A
    def m: Object = { class B; new B }
    val f: Object = { class C; new C }
    val g = () => { class D; new D }
    List[Object](new A, m, f, g())
  }
  def met = () => {
    class E
    def m: Object = { class F; new F }
    val f: Object = { class G; new G }
    val g = () => { class H; new H }
    List[Object](new E, m, f, g())
  }
}
 
object Test extends App {
  val x = new C().fun.apply() ::: new C().met.apply()
  val results = x.map(_.getClass).map(cls => (cls, cls.getEnclosingClass, cls.getEnclosingMethod))
  println(results.mkString("\n"))
}
