/*
 * filter: inliner warnings;
 */
import java.lang.Thread.holdsLock
import scala.collection.mutable.StringBuilder

object Util {
  def checkLocks(held: AnyRef*)(notHeld: AnyRef*) = {
    val sb = new StringBuilder
    for (lock <- held) {
      sb.append(if (holdsLock(lock)) '.' else '!')
    }
    print("%5s|" format sb)

    sb.clear()
    for (lock <- notHeld) {
      sb.append(if (holdsLock(lock)) '!' else '.')
    }
    print("%-15s " format sb)

    (held forall holdsLock) && !(notHeld exists holdsLock)
  }
}

class C1 {
  import Util._

  val lock = new AnyRef

  def f1 = synchronized { checkLocks(this)(this.getClass) }
  @inline final def fi = synchronized { checkLocks(this)(this.getClass) }
  val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, fv, fv.getClass) }
  def ff = {
    lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(ffv, ffv.getClass, lock.getClass) }
    ffv(this)
  }
  def fl = {
    lazy val flv = synchronized { checkLocks(this)(this.getClass) }
    flv
  }
  def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass) }

  def g1 = checkLocks()(this, this.getClass)
  @inline final def gi = checkLocks()(this, this.getClass)
  val gv: () => Boolean = () => checkLocks()(this, this.getClass, gv, gv.getClass)
  def gf = {
    lazy val gfv: AnyRef => Boolean = lock => checkLocks()(C1.this, gfv, gfv.getClass, lock, lock.getClass)
    gfv(this)
  }
  def gl = {
    lazy val glv = checkLocks()(this, this.getClass)
    glv
  }

  class C {
    def f1 = synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass) }
    @inline final def fi = synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass) }
    val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass, fv, fv.getClass) }
    def ff = {
      lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(ffv, ffv.getClass, lock.getClass, C1.this, C1.this.getClass) }
      ffv(this)
    }
    def fl = {
      lazy val flv = synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass) }
      flv
    }
    def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, C1.this, C1.this.getClass) }
    def fn = C1.this.synchronized { checkLocks(C1.this)(C1.this.getClass, this, this.getClass) }

    def g1 = checkLocks()(this, this.getClass, C1.this, C1.this.getClass)
    @inline final def gi = checkLocks()(this, this.getClass, C1.this, C1.this.getClass)
    val gv: () => Boolean = () => checkLocks()(this, this.getClass, C1.this, C1.this.getClass, gv, gv.getClass)
    def gf = {
      lazy val gfv: AnyRef => Boolean = lock => checkLocks()(gfv, gfv.getClass, lock, lock.getClass, C1.this, C1.this.getClass)
      gfv(this)
    }
    def gl = {
      lazy val glv = checkLocks()(this, this.getClass, C1.this, C1.this.getClass)
      glv
    }
  }
  val c = new C

  object O {
    def f1 = synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass) }
    @inline final def fi = synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass) }
    val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, fv, fv.getClass, C1.this, C1.this.getClass) }
    def ff = {
      lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(lock.getClass, ffv, ffv.getClass, C1.this, C1.this.getClass) }
      ffv(this)
    }
    def fl = {
      lazy val flv = synchronized { checkLocks(this)(this.getClass, C1.this, C1.this.getClass) }
      flv
    }
    def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, C1.this, C1.this.getClass) }
    def fn = C1.this.synchronized { checkLocks(C1.this)(C1.this.getClass, this, this.getClass) }

    def g1 = checkLocks()(this, this.getClass, C1.this, C1.this.getClass)
    @inline final def gi = checkLocks()(this, this.getClass, C1.this, C1.this.getClass)
    val gv: () => Boolean = () => checkLocks()(this, this.getClass, gv, gv.getClass, C1.this, C1.this.getClass)
    def gf = {
      lazy val gfv: AnyRef => Boolean = lock => checkLocks()(lock, lock.getClass, gfv, gfv.getClass, C1.this, C1.this.getClass)
      gfv(this)
    }
    def gl = {
      lazy val glv = checkLocks()(this, this.getClass, C1.this, C1.this.getClass)
      glv
    }
  }
}

object O1 {
  import Util._

  val lock = new AnyRef

  def f1 = synchronized { checkLocks(this)(this.getClass) }
  @inline final def fi = synchronized { checkLocks(this)(this.getClass) }
  val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, fv, fv.getClass) }
  def ff = {
    lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(ffv, ffv.getClass, lock.getClass) }
    ffv(this)
  }
  def fl = {
    lazy val flv = synchronized { checkLocks(this)(this.getClass) }
    flv
  }
  def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass) }

  def g1 = checkLocks()(this, this.getClass)
  @inline final def gi = checkLocks()(this, this.getClass)
  val gv: () => Boolean = () => checkLocks()(this, this.getClass, gv, gv.getClass)
  def gf = {
    lazy val gfv: AnyRef => Boolean = lock => checkLocks()(gfv, gfv.getClass, lock, lock.getClass)
    gfv(this)
  }
  def gl = {
    lazy val glv = checkLocks()(this, this.getClass)
    glv
  }

  class C {
    def f1 = synchronized { checkLocks(this)(this.getClass, O1, O1.getClass) }
    @inline final def fi = synchronized { checkLocks(this)(this.getClass, O1, O1.getClass) }
    val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, O1, O1.getClass, fv, fv.getClass) }
    def ff = {
      lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(ffv, ffv.getClass, lock.getClass, O1, O1.getClass) }
      ffv(this)
    }
    def fl = {
      lazy val flv = synchronized { checkLocks(this)(this.getClass, O1, O1.getClass) }
      flv
    }
    def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, O1, O1.getClass) }
    def fn = O1.synchronized { checkLocks(O1)(O1.getClass, this, this.getClass) }

    def g1 = checkLocks()(this, this.getClass, O1, O1.getClass)
    @inline final def gi = checkLocks()(this, this.getClass, O1, O1.getClass)
    val gv: () => Boolean = () => checkLocks()(this, this.getClass, O1, O1.getClass, gv, gv.getClass)
    def gf = {
      lazy val gfv: AnyRef => Boolean = lock => checkLocks()(gfv, gfv.getClass, lock, lock.getClass, O1, O1.getClass)
      gfv(this)
    }
    def gl = {
      lazy val glv = checkLocks()(this, this.getClass, O1, O1.getClass)
      glv
    }
  }
  val c = new C

  object O {
    def f1 = synchronized { checkLocks(this)(this.getClass, O1, O1.getClass) }
    @inline final def fi = synchronized { checkLocks(this)(this.getClass, O1, O1.getClass) }
    val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, fv, fv.getClass, O1, O1.getClass) }
    def ff = {
      lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(lock.getClass, ffv, ffv.getClass, O1, O1.getClass) }
      ffv(this)
    }
    def fl = {
      lazy val flv = synchronized { checkLocks(this)(this.getClass, O1, O1.getClass) }
      flv
    }
    def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, O1, O1.getClass) }
    def fn = O1.synchronized { checkLocks(O1)(O1.getClass, this, this.getClass) }

    def g1 = checkLocks()(this, this.getClass, O1, O1.getClass)
    @inline final def gi = checkLocks()(this, this.getClass, O1, O1.getClass)
    val gv: () => Boolean = () => checkLocks()(this, this.getClass, gv, gv.getClass, O1, O1.getClass)
    def gf = {
      lazy val gfv: AnyRef => Boolean = lock => checkLocks()(lock, lock.getClass, gfv, gfv.getClass, O1, O1.getClass)
      gfv(this)
    }
    def gl = {
      lazy val glv = checkLocks()(this, this.getClass, O1, O1.getClass)
      glv
    }
  }
}

trait T {
  import Util._

  val lock = new AnyRef

  def f1 = synchronized { checkLocks(this)(this.getClass, classOf[T], classOf[C2], O2.getClass) }
  @inline final def fi = synchronized { checkLocks(this)(this.getClass, classOf[T], classOf[C2], O2.getClass) }
  val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, fv, fv.getClass, classOf[T], classOf[C2], O2.getClass) }
  def ff = {
    lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(ffv, ffv.getClass, lock.getClass, classOf[T], classOf[C2], O2.getClass) }
    ffv(this)
  }
  def fl = {
    lazy val flv = synchronized { checkLocks(this)(this.getClass, classOf[T], classOf[C2], O2.getClass) }
    flv
  }
  def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, classOf[T], classOf[C2], O2.getClass) }

  def g1 = checkLocks()(this, this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
  @inline final def gi = checkLocks()(this, this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
  val gv: () => Boolean = () => checkLocks()(this, this.getClass, gv, gv.getClass, classOf[T], classOf[C2], O2, O2.getClass)
  def gf = {
    lazy val gfv: AnyRef => Boolean = lock => checkLocks()(gfv, gfv.getClass, lock, lock.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    gfv(this)
  }
  def gl = {
    lazy val glv = checkLocks()(this, this.getClass, classOf[T], classOf[C2], O2.getClass)
    glv
  }

  class C {
    def f1 = synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    @inline final def fi = synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, fv, fv.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    def ff = {
      lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(ffv, ffv.getClass, lock.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
      ffv(this)
    }
    def fl = {
      lazy val flv = synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
      flv
    }
    def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    def fn = T.this.synchronized { checkLocks(T.this)(T.this.getClass, this, this.getClass, classOf[T], classOf[C2], O2.getClass) }

    def g1 = checkLocks()(this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    @inline final def gi = checkLocks()(this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    val gv: () => Boolean = () => checkLocks()(this, this.getClass, T.this, T.this.getClass, gv, gv.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    def gf = {
      lazy val gfv: AnyRef => Boolean = lock => checkLocks()(gfv, gfv.getClass, lock, lock.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
      gfv(this)
    }
    def gl = {
      lazy val glv = checkLocks()(this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
      glv
    }
  }
  val c = new C

  object O {
    def f1 = synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    @inline final def fi = synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    val fv: () => Boolean = () => synchronized { checkLocks(this)(this.getClass, fv, fv.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    def ff = {
      lazy val ffv: AnyRef => Boolean = lock => synchronized { checkLocks(lock)(lock.getClass, ffv, ffv.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
      ffv(this)
    }
    def fl = {
      lazy val flv = synchronized { checkLocks(this)(this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
      flv
    }
    def fo = lock.synchronized { checkLocks(lock)(lock.getClass, this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass) }
    def fn = T.this.synchronized { checkLocks(T.this)(T.this.getClass, this, this.getClass, classOf[T], classOf[C2], O2.getClass) }

    def g1 = checkLocks()(this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    @inline final def gi = checkLocks()(this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    val gv: () => Boolean = () => checkLocks()(this, this.getClass, gv, gv.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
    def gf = {
      lazy val gfv: AnyRef => Boolean = lock => checkLocks()(lock, lock.getClass, gfv, gfv.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
      gfv(this)
    }
    def gl = {
      lazy val glv = checkLocks()(this, this.getClass, T.this, T.this.getClass, classOf[T], classOf[C2], O2, O2.getClass)
      glv
    }
  }
}

class C2 extends T
object O2 extends T

object Test extends App {
  def check(name: String, result: Boolean) {
    println("%-10s %s" format (name +":", if (result) "OK" else "FAILED"))
  }

  val c1 = new C1
  check("c1.f1",   c1.f1)
  check("c1.fi",   c1.fi)
  check("c1.fv",   c1.fv())
  check("c1.ff",   c1.ff)
  check("c1.fl",   c1.fl)
  check("c1.fo",   c1.fo)
  check("c1.g1",   c1.g1)
  check("c1.gi",   c1.gi)
  check("c1.gv",   c1.gv())
  check("c1.gf",   c1.gf)
//  check("c1.gl",   c1.gl)   // FIXME *.gl are failing because of the issue described in SUGGEST-11

  check("c1.c.f1",   c1.c.f1)
  check("c1.c.fi",   c1.c.fi)
  check("c1.c.fv",   c1.c.fv())
  check("c1.c.ff",   c1.c.ff)
  check("c1.c.fl",   c1.c.fl)
  check("c1.c.fo",   c1.c.fo)
  check("c1.c.fn",   c1.c.fn)
  check("c1.c.g1",   c1.c.g1)
  check("c1.c.gi",   c1.c.gi)
  check("c1.c.gv",   c1.c.gv())
  check("c1.c.gf",   c1.c.gf)
//  check("c1.c.gl",   c1.c.gl)

  check("c1.O.f1",   c1.O.f1)
  check("c1.O.fi",   c1.O.fi)
  check("c1.O.fv",   c1.O.fv())
  check("c1.O.ff",   c1.O.ff)
  check("c1.O.fl",   c1.O.fl)
  check("c1.O.fo",   c1.O.fo)
  check("c1.O.fn",   c1.O.fn)
  check("c1.O.g1",   c1.O.g1)
  check("c1.O.gi",   c1.O.gi)
  check("c1.O.gv",   c1.O.gv())
  check("c1.O.gf",   c1.O.gf)
//  check("c1.O.gl",   c1.O.gl)

  check("O1.f1",   O1.f1)
  check("O1.fi",   O1.fi)
  check("O1.fv",   O1.fv())
  check("O1.ff",   O1.ff)
  check("O1.fl",   O1.fl)
  check("O1.fo",   O1.fo)
  check("O1.g1",   O1.g1)
  check("O1.gi",   O1.gi)
  check("O1.gv",   O1.gv())
  check("O1.gf",   O1.gf)
//  check("O1.gl",   O1.gl)

  check("O1.c.f1",   O1.c.f1)
  check("O1.c.fi",   O1.c.fi)
  check("O1.c.fv",   O1.c.fv())
  check("O1.c.ff",   O1.c.ff)
  check("O1.c.fl",   O1.c.fl)
  check("O1.c.fo",   O1.c.fo)
  check("O1.c.fn",   O1.c.fn)
  check("O1.c.g1",   O1.c.g1)
  check("O1.c.gi",   O1.c.gi)
  check("O1.c.gv",   O1.c.gv())
  check("O1.c.gf",   O1.c.gf)
//  check("O1.c.gl",   O1.c.gl)

  check("O1.O.f1",   O1.O.f1)
  check("O1.O.fi",   O1.O.fi)
  check("O1.O.fv",   O1.O.fv())
  check("O1.O.ff",   O1.O.ff)
  check("O1.O.fl",   O1.O.fl)
  check("O1.O.fo",   O1.O.fo)
  check("O1.O.fn",   O1.O.fn)
  check("O1.O.g1",   O1.O.g1)
  check("O1.O.gi",   O1.O.gi)
  check("O1.O.gv",   O1.O.gv())
  check("O1.O.gf",   O1.O.gf)
//  check("O1.O.gl",   O1.O.gl)

  val c2 = new C2
  check("c2.f1",   c2.f1)
  check("c2.fi",   c2.fi)
  check("c2.fv",   c2.fv())
  check("c2.ff",   c2.ff)
  check("c2.fl",   c2.fl)
  check("c2.fo",   c2.fo)
  check("c2.g1",   c2.g1)
  check("c2.gi",   c2.gi)
  check("c2.gv",   c2.gv())
  check("c2.gf",   c2.gf)
//  check("c2.gl",   c2.gl)

  check("c2.c.f1",   c2.c.f1)
  check("c2.c.fi",   c2.c.fi)
  check("c2.c.fv",   c2.c.fv())
  check("c2.c.ff",   c2.c.ff)
  check("c2.c.fl",   c2.c.fl)
  check("c2.c.fo",   c2.c.fo)
  check("c2.c.fn",   c2.c.fn)
  check("c2.c.g1",   c2.c.g1)
  check("c2.c.gi",   c2.c.gi)
  check("c2.c.gv",   c2.c.gv())
  check("c2.c.gf",   c2.c.gf)
//  check("c2.c.gl",   c2.c.gl)

  check("c2.O.f1",   c2.O.f1)
  check("c2.O.fi",   c2.O.fi)
  check("c2.O.fv",   c2.O.fv())
  check("c2.O.ff",   c2.O.ff)
  check("c2.O.fl",   c2.O.fl)
  check("c2.O.fo",   c2.O.fo)
  check("c2.O.fn",   c2.O.fn)
  check("c2.O.g1",   c2.O.g1)
  check("c2.O.gi",   c2.O.gi)
  check("c2.O.gv",   c2.O.gv())
  check("c2.O.gf",   c2.O.gf)
//  check("c2.O.gl",   c2.O.gl)

  check("O2.f1",   O2.f1)
  check("O2.fi",   O2.fi)
  check("O2.fv",   O2.fv())
  check("O2.ff",   O2.ff)
  check("O2.fl",   O2.fl)
  check("O2.fo",   O2.fo)
  check("O2.g1",   O2.g1)
  check("O2.gi",   O2.gi)
  check("O2.gv",   O2.gv())
  check("O2.gf",   O2.gf)
//  check("O2.gl",   O2.gl)

  check("O2.c.f1",   O2.c.f1)
  check("O2.c.fi",   O2.c.fi)
  check("O2.c.fv",   O2.c.fv())
  check("O2.c.ff",   O2.c.ff)
  check("O2.c.fl",   O2.c.fl)
  check("O2.c.fo",   O2.c.fo)
  check("O2.c.fn",   O2.c.fn)
  check("O2.c.g1",   O2.c.g1)
  check("O2.c.gi",   O2.c.gi)
  check("O2.c.gv",   O2.c.gv())
  check("O2.c.gf",   O2.c.gf)
//  check("O2.c.gl",   O2.c.gl)

  check("O2.O.f1",   O2.O.f1)
  check("O2.O.fi",   O2.O.fi)
  check("O2.O.fv",   O2.O.fv())
  check("O2.O.ff",   O2.O.ff)
  check("O2.O.fl",   O2.O.fl)
  check("O2.O.fo",   O2.O.fo)
  check("O2.O.fn",   O2.O.fn)
  check("O2.O.g1",   O2.O.g1)
  check("O2.O.gi",   O2.O.gi)
  check("O2.O.gv",   O2.O.gv())
  check("O2.O.gf",   O2.O.gf)
//  check("O2.O.gl",   O2.O.gl)
}
