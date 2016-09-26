object log {
  val b = new collection.mutable.StringBuilder
  def apply(s: Any): Unit = b.append(s)
  def check(s: String) = {
    val bs = b.toString
    assert(s == bs, bs)
    b.clear()
  }
}

package t9697 {
  abstract class WA extends DelayedInit {
    override def delayedInit(x: => Unit): Unit = x
    val waField = "4"
  }

  class C {
    def b(s: String) = log(s)
    val cField = "1"

    {
      val dummyLocal = "2"
      new WA {
        val anonField = "3"
        b(cField)
        b(dummyLocal)
        b(anonField)
        b(waField)
      }
    }
  }
}

package sd229 {
  class Broken {
    def is(ee: AnyRef) = {
      new Delayed {
        log(ee)
      }
    }
  }

  class Delayed extends DelayedInit {
    def delayedInit(x: => Unit): Unit = x
  }
}


// already fixed in 2.11.8, crashes in 2.10.6
package t4683a {
  class A { log("a") }
  class B { log("b") }
  class Bug extends DelayedInit {
    log("bug")
    def foo(a: A): B = new B
    def delayedInit(init: => Unit): Unit = init
  }
}

// already fixed in 2.12.0-RC1, crashes in 2.11.8
package t4683b {
  class Entity extends DelayedInit {
    def delayedInit(x: => Unit): Unit = x
    
    class Field
    
    protected def EntityField[T <: Entity: reflect.ClassTag] = new Field
    
    def find[T <: Entity: reflect.ClassTag] {
      Nil.map(dbo => {
        class EntityHolder extends Entity {
          val entity = EntityField[T]
        }
      })
      log("find")
    }
  }
}

package t4683c {
  trait T extends DelayedInit {
    def delayedInit(body: => Unit) = {
      log("init")
      body
    }
  }
}

package t4683d {
  class C extends DelayedInit {
    def delayedInit(body: => Unit): Unit = body
  }
  class Injector {
    def test: Object = {
      val name = "k"
      class crash extends C {
        log(name)
      }
      new crash()
    }
  }
}

object Test extends App {
  new t9697.C()
  log.check("1234")

  new sd229.Broken().is("hi")
  log.check("hi")

  val a: t4683a.A = new t4683a.A
  var b: t4683a.B = null
  new t4683a.Bug {
    val b = foo(a)
  }
  log.check("abugb")

  new t4683b.Entity().find[t4683b.Entity]
  log.check("find")

  val f = (p1: Int) => new t4683c.T { log(p1) }
  f(5)
  log.check("init5")

  new t4683d.Injector().test
  log.check("k")
}
