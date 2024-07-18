//> using options -Xlint:deprecation
//> using javaOpt -Ddummy=fresh_jvm_needed_to_test_security_manager
//> using filter WARNING.*
// for now, ignore warnings due to reflective invocation
import java.security._

import scala.annotation.nowarn
import scala.language.reflectiveCalls
import scala.util.Properties.isJavaAtLeast

// SecurityManager is deprecated on JDK 17, so we sprinkle `@nowarn` around

object Test {
  trait Bar { def bar(): Unit }

  @nowarn("cat=deprecation")
  object Mgr extends SecurityManager {
    def allowedProperty(name: String) =
      name == "sun.net.inetaddr.ttl" ||
        name == "scala.control.noTraceSuppression" // module initializer for object NoStackTrace

    override def checkPermission(perm: Permission) = perm match {
      case _: java.lang.RuntimePermission                                                   => ()
      case _: java.io.FilePermission                                                        => ()
      case x: java.security.SecurityPermission if x.getName contains ".networkaddress."     => () // generality ftw
      case x: java.util.PropertyPermission if allowedProperty(x.getName)                    => ()
      case _: java.lang.reflect.ReflectPermission                                           => () // needed for LambdaMetaFactory
      case _                                                                                => super.checkPermission(perm)
    }
  }

  def t1() = {
    @nowarn("cat=deprecation")
    val p = Runtime.getRuntime().exec("ls");
    type Destroyable = { def destroy() : Unit }
    def doDestroy( obj : Destroyable ) : Unit = obj.destroy();
    doDestroy( p );
  }

  @nowarn("cat=deprecation")
  def t2() = if (!isJavaAtLeast(18)) {
    System.setSecurityManager(Mgr)
    var count = 0

    val b = new Bar { def bar() = count += 1 }
    b.bar()

    val structural = b.asInstanceOf[{ def bar(): Unit }]
    structural.bar()

    assert(count == 2, "Expected 2 invocations")
  }

  def main(args: Array[String]): Unit = {
    // figuring this will otherwise break on windows
    try t1()
    catch { case _: java.io.IOException => () }

    t2()
  }
}
