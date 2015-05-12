import java.security._

import scala.language.{ reflectiveCalls }

object Test {
  trait Bar { def bar: Unit }

  object Mgr extends SecurityManager {
    override def checkPermission(perm: Permission) = perm match {
      case _: java.lang.RuntimePermission                                                   => ()
      case _: java.io.FilePermission                                                        => ()
      case x: java.security.SecurityPermission if x.getName contains ".networkaddress."     => () // generality ftw
      case x: java.util.PropertyPermission if x.getName == "sun.net.inetaddr.ttl"           => ()
      case _: java.lang.reflect.ReflectPermission                                           => () // needed for LambdaMetaFactory
      case _                                                                                => super.checkPermission(perm)
    }
  }

  def t1() = {
    val p = Runtime.getRuntime().exec("ls");
    type Destroyable = { def destroy() : Unit }
    def doDestroy( obj : Destroyable ) : Unit = obj.destroy();
    doDestroy( p );
  }
  def t2() = {
    System.setSecurityManager(Mgr)

    val b = new Bar { def bar = println("bar") }
    b.bar

    val structural = b.asInstanceOf[{ def bar: Unit }]
    structural.bar
  }

  def main(args: Array[String]) {
    // figuring this will otherwise break on windows
    try t1()
    catch { case _: java.io.IOException => () }

    t2()
  }
}
