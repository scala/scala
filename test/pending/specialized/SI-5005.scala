import scala.tools.partest._
import java.io._



// I think this may be due to a bug in partest where it uses some other version
// of the scala-library.jar - _hashCode is in line 202 currently, not 212!
//
//  [partest] testing: [...]/files/specialized/SI-5005.scala                        [FAILED]
//  [partest] java.lang.NoClassDefFoundError: scala/util/MurmurHash3$
//  [partest] java.lang.NoClassDefFoundError: scala/util/MurmurHash3$
//  [partest] 	at scala.runtime.ScalaRunTime$._hashCode(ScalaRunTime.scala:212)
object Test extends DirectTest {
  
  override def extraSettings: String = "-usejavacp -Xprint:spec -optimize -Ylog:inliner -d " + testOutput.path

  override def code = """
    class C2[@specialized(Boolean) U]() {
      @inline final def apply(x: U): U = x
    }

    class B {
      (new C2[Boolean]())(true)
    }
  """

  override def show(): Unit = {
    // redirect err to out, for inliner log
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    System.setErr(prevErr)
  }

  override def isDebug = false // so we don't get the newSettings warning
}
