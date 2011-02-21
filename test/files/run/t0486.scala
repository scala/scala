object Test extends App {
  import scala.xml._

  def wsdlTemplate1(serviceName: String): Node =
    <wsdl:definitions name={serviceName} xmlns:tns = { "target1" } >
    </wsdl:definitions>;

  def wsdlTemplate2(serviceName: String, targetNamespace: String): Node =
    <wsdl:definitions name={serviceName} xmlns:tns = { targetNamespace } >
    </wsdl:definitions>;

  def wsdlTemplate3(serviceName: String): Node =
    <wsdl:definitions name={serviceName} xmlns:tns = { Text("target3") } >
    </wsdl:definitions>;

  def wsdlTemplate4(serviceName: String, targetNamespace: () => String): Node =
    <wsdl:definitions name={serviceName} xmlns:tns = { targetNamespace() } >
    </wsdl:definitions>;

  println(wsdlTemplate1("service1"))
  println(wsdlTemplate2("service2", "target2"))
  println(wsdlTemplate3("service3"))
  println(wsdlTemplate4("service4", () => "target4"))
}
