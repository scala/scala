package p

@scala.beans.BeanInfo
class C {
  val x: Int = 0
  var y: String = ""
  var z: List[_] = Nil
  def f: C = ???
}
