object Reflection1 extends Application {
  case class Element(name: String)
  println(reflect.Code.lift({val e = Element("someName"); e}).tree)
}
