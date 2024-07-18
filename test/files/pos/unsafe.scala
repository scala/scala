
//> using options --release:8 -Yrelease:sun.misc

import sun.misc.Unsafe

class C {
  val f = classOf[Unsafe].getDeclaredField("theUnsafe")
  f.setAccessible(true)
  val unsafe = f.get(null).asInstanceOf[Unsafe]

  val k = unsafe.allocateInstance(classOf[K]).asInstanceOf[K]
  assert(k.value == 0)
}

class K {
  val value = 42
}

object Test extends App {
  new C
}
