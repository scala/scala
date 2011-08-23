object Reflection1 extends App {
  case class Element(name: String)
  println(reflect.Code.lift({val e = Element("someName"); e}).tree)
}
