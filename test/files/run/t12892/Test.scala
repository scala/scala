import scala.tools.testkit.AssertUtil._

object Test extends App {
  // OK
  def t(b: Ba): Ba = b.m
  def t(c: Ca): Ca = c.m

  // TODO should compile
  // def t(d: Da): Da = d.m

  // TODO Xb should not compile
  assertThrows[java.lang.AbstractMethodError] {
    val t1 = new Xb().m
  }
  val t2 = new Xc().m

  // OK error: type mismatch. found Function, required Ba. Ba is not a SAM.
  // val ba: Ba = (x: Int) => x + 1
  // val bb: Bb = (x: Int) => x + 1

  // OK
  val bc: Bc = (x: Int) => x + 1
  val comp: java.util.Comparator[Int] = _ - _

  val t3 = bc.m

  // OK error: type mismatch. found Function, required Ca. Ca is not a SAM.
  // val ca: Ca = (x: Int) => x + 1
  // val cb: Cb = (x: Int) => x + 1

  // OK
  val cc: Cc = (x: Int) => x + 1

  val t4 = cc.m

  // TODO should not compile
  val da: Da = (x: Int) => x + 1

  // TODO should not compile
  val db: Db = (x: Int) => x + 1

  // OK
  val dc: Dc = (x: Int) => x + 1

  // TODO: da should not compie
  assertThrows[java.lang.AbstractMethodError] {
    val t5 = da.m
  }
  // TODO: tb should not compile
  assertThrows[java.lang.AbstractMethodError] {
    val t6 = db.m
  }
  val t7 = dc.m


  // OK error: incompatible type in overriding
  // class Xa extends Ba { def sam(i: Int) = i }

  // TODO should not compile
  class Xb extends Bb {
    def sam(i: Int) = i
  }

  // OK
  class Xc extends Bc {
    def sam(i: Int) = i
  }

  // OK error: no implementation found for m
  // class Ya extends Ca { def sam(i: Int) = i }

  // OK error: no implementation found for m
  // class Yb extends Cb { def sam(i: Int) = i }

  // OK error: no implementation found for equals
  // class Yc extends Cc { def sam(i: Int) = i }

  // OK error: no implementation found for m
  // class Za extends Da { def sam(i: Int) = i }

  // OK error: no implementation found for m
  // class Zb extends Db { def sam(i: Int) = i }

  // OK error: no implementation found for equals
  // class Zc extends Dc { def sam(i: Int) = i }

}
