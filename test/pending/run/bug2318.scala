import java.security._

object Test {
  trait Bar { def bar: Unit }
  
  object Mgr extends SecurityManager {
    override def checkPermission(perm: Permission) = perm match {
      case _: java.lang.RuntimePermission                                                   => ()
      case _: java.io.FilePermission                                                        => ()
      case x: java.security.AccessControlException if x.getName contains ".networkaddress." => () // generality ftw
      case _                                                                                => super.checkPermission(perm)
    }
  }
  
  def bug1() = {
    val p = Runtime.getRuntime().exec("ls");
    type Destroyable = { def destroy() : Unit }
    def doDestroy( obj : Destroyable ) : Unit = obj.destroy(); 
    doDestroy( p );
  }
  def bug2() = {
    System.setSecurityManager(Mgr)

    val b = new Bar { def bar = println("bar") }
    b.bar

    val structural = b.asInstanceOf[{ def bar: Unit }]
    structural.bar
  }
  
  def main(args: Array[String]) {
    // figuring this will otherwise break on windows
    try bug1()
    catch { case _: java.io.IOException => () }
    
    bug2()
  }
}
