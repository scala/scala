import java.lang.reflect.Modifier

import scala.annotation.strictfp

class Foo extends (() => Unit) {
  def apply(): Unit = synchronized {
    // we're in a specialized subclass
    assert(Thread.currentThread.getStackTrace.apply(1).getMethodName == "apply$mcV$sp")
    assert(Thread.holdsLock(this))
  }
}

class Bar extends (() => Unit) {
  @strictfp def apply(): Unit = synchronized {
    // we're in a specialized subclass
    assert(Thread.currentThread.getStackTrace.apply(1).getMethodName == "apply$mcV$sp")
    assert(Thread.holdsLock(this))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new Foo().apply()
    
    val m = classOf[Bar].getDeclaredMethod("apply$mcV$sp")
    assert(Modifier.isStrict(m.getModifiers))
  }
}
