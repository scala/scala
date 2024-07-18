//> using options -Ystop-after:parser
//
object foo {
  val xml = 
    <wsdl:definitions name={ serviceName } xmlns:tns={ new _root_.scala.xml.Text("target3") }>
    </wsdl:definitions>;
}
