import scala.tools.partest._
import java.io.{Console => _, _}

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -Xprint-pos -Xprint:patmat -Ystop-after:patmat -d " + testOutput.path

  override def code =
    """
    |class C {                                                                                        //
    |  def commonSubPattern(x: Any) = {                                                               //
    |    x match {                                                                                    //
    |      case _: Option[_] =>                                                                       //
    |      case s: String if s == "4" =>                                                              //
    |         s.hashCode                                                                              //
    |      case s: String if s == "6" =>                                                              //
    |         s.hashCode                                                                              //
    |    }                                                                                            //
    |  }                                                                                              //
    |  def extractor(x: Any) = x match {                                                              //
    |      case Product2(a, b) =>                                                                     //
    |         a                                                                                       //
    |    }                                                                                            //
    |  def swatch = {                                                                                 //
    |    try {                                                                                        //
    |      toString                                                                                   //
    |    } catch {                                                                                    //
    |      case t: Throwable                                                                          //
    |        if "".isEmpty =>                                                                         //
    |           t.toString                                                                            //
    |    }                                                                                            //
    |  }                                                                                              //
    |}
    |""".stripMargin


  override def show(): Unit = {
    Console.withErr(System.out) {
      compile()
    }
  }
}