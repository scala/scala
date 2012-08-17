


import java.lang.reflect.Modifier
import annotation.static
import reflect._



/* TEST 1 */

/* A @static-annotated field in the companion object should yield
 * a static field in its companion class.
 */
object Foo {
  @static val bar = 17
}


class Foo


trait Check {
  def checkStatic(cls: Class[_]) {
    cls.getDeclaredFields.find(_.getName == "bar") match {
      case Some(f) =>
        assert(Modifier.isStatic(f.getModifiers), "no static modifier")
      case None =>
        assert(false, "no static field bar in class")
    }
  }
  
  def test(): Unit
}


object Test1 extends Check {
  def test() {
    checkStatic(classOf[Foo])
    assert(Foo.bar == 17, "Companion object field should be 17.")
  }
}


/* TEST 2 */

class Foo2


/** The order of declaring the class and its companion is inverted now. */
object Foo2 {
  @static val bar = 199
}


object Test2 extends Check {
  def test() {
    checkStatic(Class.forName("Foo3"))
    assert(Foo3.bar == 1984, "Companion object field should be 1984.")
  }
}


/* TEST 3 */

/** The case where there is no explicit companion class */
object Foo3 {
  @static val bar = 1984
}


object Test3 extends Check {
  def test() {
    checkStatic(Class.forName("Foo3"))
    assert(Foo3.bar == 1984, "Companion object field should be 1984.")
  }
}


/* TEST 4 */

/** We want to be able to generate atomic reference field updaters on the companion object
 *  so that they are created only once per class declaration, but we want them to actually
 *  be initialize __in the static initializer of the class itself__.
 *  This is extremely important, because otherwise the creation of the ARFU fails, since it uses
 *  trickery to detect the caller and compare it to the owner of the field being modified.
 *  Previously, this used to be circumvented through the use of Java base classes. A pain.
 */
class ArfuTarget {
  @volatile var strfield = ArfuTarget.STR
  
  def CAS(ov: String, nv: String): Boolean = {
    ArfuTarget.arfu.compareAndSet(this, ov, nv)
  }
}


object ArfuTarget {
  @static val arfu = java.util.concurrent.atomic.AtomicReferenceFieldUpdater.newUpdater(classOf[ArfuTarget], classOf[String], "strfield")
  val STR = "Some string"
}


object Test4 extends Check {
  def checkArfu() {
    val at = new ArfuTarget
    assert(at.strfield == ArfuTarget.STR)
    at.CAS(ArfuTarget.STR, null)
    assert(at.strfield == null)
  }
  
  def test() {
    checkArfu()
  }
}


/* TEST 5 */

/** Although our main use-case is to use final static fields, we should be able to use non-final too.
 *  Here we set the static field of the class by using the setters in the companion object.
 *  It is legal to do so using the reference to `Foo` directly (in which case the callsites
 *  are rewritten to access the static field directly), or through an interface `Var` (in
 *  which case the getter and the setter for `field` access the static field in `Var`).
 */
trait Var {
  var field: Int
}

object VarHolder extends Var {
  @static var field = 1
}


object Test5 extends Check {
  def test() {
    assert(VarHolder.field == 1)
    VarHolder.field = 2
    assert(VarHolder.field == 2)
    val vh: Var = VarHolder
    vh.field = 3
    assert(vh.field == 3)
  }
}


/* TEST 6 */

/** Here we test flattening the static ctor body and changing the owners of local definitions. */
object Foo6 {
  var companionField = 101
  @static val staticField = {
    val intermediate = companionField + 1
    intermediate * 2
  }
}


object Test6 extends Check {
  def test() {
    assert(Foo6.staticField == 204)
  }
}



/* TEST 7 */

/** Here we test objects nested in top-level objects */
object Foo7 {
  object AndHisFriend {
    @static val bar = "string"
  }
  class AndHisFriend

  object AndHisLonelyFriend {
    @static val bar = "another"
  }
}


object Test7 extends Check {
  def test() {
    checkStatic(classOf[Foo7.AndHisFriend])
    assert(Foo7.AndHisFriend.bar == "string")

    checkStatic(Class.forName("Foo7$AndHisLonelyFriend"))
    assert(Foo7.AndHisLonelyFriend.bar == "another")
  }
}



/* TEST 8 */

object Foo8 {
  @static val field = 7
  
  val function: () => Int = () => {
    field + 1
  }
  
  val anon = new Runnable {
    def run() {
      assert(field == 7, "runnable asserting field is 7")
    }
  }
  
  @static var mutable = 10
  
  val mutation: () => Unit = () => {
    mutable += 1
  }
}

object Test8 {
  def test() {
    assert(Foo8.function() == 8, "function must return 8")
    Foo8.anon.run()
    assert(Foo8.mutable == 10, "mutable is 10")
    Foo8.mutation()
    assert(Foo8.mutable == 11, "mutable is 11")
    Foo8.mutation()
    assert(Foo8.mutable == 12, "mutable is 12")
  }
}




/* main */

object Test {
  
  def main(args: Array[String]) {
    Test1.test()
    Test2.test()
    Test3.test()
    Test4.test()
    Test5.test()
    Test6.test()
    Test7.test()
    Test8.test()
  }
  
}






