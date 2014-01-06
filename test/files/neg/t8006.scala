object X {
  val d = new D
  d.meth(value1 = 10, value2 = 100) // two arguments here, but only one is allowed
}
import language.dynamics
class D extends Dynamic {
  def applyDynamicNamed(name: String)(value: (String, Any)) = name
}