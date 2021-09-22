import scala.tools.partest._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Yshow-trees-stringified -Vprint:constructors"

  override def code =
    """
      | class Foo[S <: AnyRef](val s: S) extends AnyVal            
      |  
      | object Test extends App {                                                             
      |                                                             
      |  def Foo[S <: AnyRef](name: S) = new Foo[name.type](name)  
      |                                                                 
      |  val foo = Foo("x")                                         
      |                                                             
      |}
      |""".stripMargin

  override def show(): Unit = compile() 
}
