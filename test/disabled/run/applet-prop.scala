import scala.tools.partest._
import java.util.PropertyPermission
import java.security.AccessControlException

class S extends javax.swing.JApplet {
  scala.collection.Traversable
}

object Test extends SecurityTest {
  val s = new S
  // lazy val TestKey = sys.SystemProperties.noTraceSuppression.key
  // def hitPerm() = new Throwable with scala.util.control.ControlThrowable { }
  // 
  // var throwing = false
  // override def propertyCheck(p: PropertyPermission): Unit = {
  //   if (p.getName == TestKey) {
  //     println("I see " + p.getName)
  //     if (throwing)
  //       throwIt(p)
  //   }
  // }
  // 
  // hitPerm()
  // securityOn()
  // hitPerm()
  // 
  // throwing = true
  //
  // val caught = 
  //   try   { hitPerm() ; false }
  //   catch { case _: AccessControlException => true }
  // 
  // assert(caught, "Should have incurred exception.")
  // throwing = false
  // hitPerm()
  // 
  // val xs = new Traversable[Int] { def foreach[U](f: Int => U) = 1 to 3 foreach f }
  // xs foreach println
}

