object Logs {
  val logBuf = new collection.mutable.StringBuilder()
  def log(m: Any): Unit = { if (logBuf.nonEmpty) logBuf.append(":"); logBuf.append(m) }
  def checkLog(expected: String): Unit = {
    val res = logBuf.toString
    assert(res == expected, s"expected:\n$expected\nfound:\n$res")
    logBuf.clear()
  }
}

import Logs._

class C {
  def getInt   : Int    = { log("getInt"); 1 }
  def getString: String = { log("getString"); "s" }
  def getUnit  : Unit   = { log("getUnit") }

  lazy val t1 = getInt
  lazy val t2 = getString
  lazy val t3 = getUnit
  checkLog("")

  def m1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  def m2 = {
    lazy val t1 = getString
    t1 + t1
  }
  def m3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("")


  val vl1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  val vl2 = {
    lazy val t1 = getString
    t1 + t1
  }
  val vl3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("getInt:getString:getUnit:():()")


  var vr1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  var vr2 = {
    lazy val t1 = getString
    t1 + t1
  }
  var vr3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("getInt:getString:getUnit:():()")


  lazy val lvl1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  lazy val lvl2 = {
    lazy val t1 = getString
    t1 + t1
  }
  lazy val lvl3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("")


  {
    lazy val t1 = getInt
    lazy val t2 = getString
    lazy val t3 = getUnit

    log(t1 + t1)
    log(t2 + t2)
    log(t3); log(t3)
  }
  checkLog("getInt:2:getString:ss:getUnit:():()")


  def run(): Unit = {
    log(t1); log(t1);
    log(t2); log(t2);
    log(t3); log(t3);
    checkLog("getInt:1:1:getString:s:s:getUnit:():()")

    log(m1); log(m1)
    log(m2); log(m2)
    log(m3); log(m3)
    checkLog("getInt:2:getInt:2:getString:ss:getString:ss:getUnit:():():():getUnit:():():()")

    log(vl1); log(vl1)
    log(vl2); log(vl2)
    log(vl3); log(vl3)
    checkLog("2:2:ss:ss:():()")

    log(vr1); log(vr1); vr1 = 393; log(vr1)
    log(vr2); log(vr2); vr2 = "h"; log(vr2)
    log(vr3); log(vr3); vr3 = () ; log(vr3)
    checkLog("2:2:393:ss:ss:h:():():()")

    log(lvl1); log(lvl1)
    log(lvl2); log(lvl2)
    log(lvl3); log(lvl3)
    checkLog("getInt:2:2:getString:ss:ss:getUnit:():():():()")
  }
}

trait T {
  def getInt   : Int    = { log("getInt"); 1 }
  def getString: String = { log("getString"); "s" }
  def getUnit  : Unit   = { log("getUnit") }

  lazy val t1 = getInt
  lazy val t2 = getString
  lazy val t3 = getUnit
  checkLog("")

  def m1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  def m2 = {
    lazy val t1 = getString
    t1 + t1
  }
  def m3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("")


  val vl1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  val vl2 = {
    lazy val t1 = getString
    t1 + t1
  }
  val vl3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("getInt:getString:getUnit:():()")


  var vr1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  var vr2 = {
    lazy val t1 = getString
    t1 + t1
  }
  var vr3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("getInt:getString:getUnit:():()")


  lazy val lvl1 = {
    lazy val t1 = getInt
    t1 + t1
  }
  lazy val lvl2 = {
    lazy val t1 = getString
    t1 + t1
  }
  lazy val lvl3 = {
    lazy val t1 = getUnit
    log(t1); log(t1)
  }
  checkLog("")


  {
    lazy val t1 = getInt
    lazy val t2 = getString
    lazy val t3 = getUnit

    log(t1 + t1)
    log(t2 + t2)
    log(t3); log(t3)
  }
  checkLog("getInt:2:getString:ss:getUnit:():()")


  def run(): Unit = {
    log(t1); log(t1);
    log(t2); log(t2);
    log(t3); log(t3);
    checkLog("getInt:1:1:getString:s:s:getUnit:():()")

    log(m1); log(m1)
    log(m2); log(m2)
    log(m3); log(m3)
    checkLog("getInt:2:getInt:2:getString:ss:getString:ss:getUnit:():():():getUnit:():():()")

    log(vl1); log(vl1)
    log(vl2); log(vl2)
    log(vl3); log(vl3)
    checkLog("2:2:ss:ss:():()")

    log(vr1); log(vr1); vr1 = 393; log(vr1)
    log(vr2); log(vr2); vr2 = "h"; log(vr2)
    log(vr3); log(vr3); vr3 = () ; log(vr3)
    checkLog("2:2:393:ss:ss:h:():():()")

    log(lvl1); log(lvl1)
    log(lvl2); log(lvl2)
    log(lvl3); log(lvl3)
    checkLog("getInt:2:2:getString:ss:ss:getUnit:():():():()")
  }
}

class D extends T

class D1 extends T {
  override lazy val t1 = { log("o-t1"); -1 }
  checkLog("")

  override def m1 = { log("o-m1"); -2 }
  override val m2 = { log("o-m2"); "n" }
  override lazy val m3 = { log("o-m3") }
  checkLog("o-m2")

  override val vl1 = { log("o-vl1"); -3 }
  checkLog("o-vl1")

  override lazy val lvl1 = { log("o-lvl1"); -4 }
  checkLog("")

  override def run(): Unit = {
    log(t1); log(t1)
    checkLog("o-t1:-1:-1")

    log(m1); log(m1)
    log(m2); log(m2)
    log(m3); log(m3)
    checkLog("o-m1:-2:o-m1:-2:n:n:o-m3:():()")

    log(vl1); log(vl1)
    checkLog("-3:-3")

    log(lvl1); log(lvl1)
    checkLog("o-lvl1:-4:-4")
  }
}

class E {
  object T { log("init T"); override def toString = "T" }
  def m = { object T { log("init T"); val x = 1 }; T.x }
  checkLog("")
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    c.run()

    val lzyComputeMethods = c.getClass.getDeclaredMethods.filter(_.getName contains "lzycompute").map(_.getName).toList.sorted
    val expComputeMethods = List("lvl1$lzycompute", "lvl2$lzycompute", "lvl3$lzycompute", "t1$lzycompute", "t1$lzycompute$1", "t1$lzycompute$10", "t1$lzycompute$11", "t1$lzycompute$12", "t1$lzycompute$13", "t1$lzycompute$2", "t1$lzycompute$3", "t1$lzycompute$4", "t1$lzycompute$5", "t1$lzycompute$6", "t1$lzycompute$7", "t1$lzycompute$8", "t1$lzycompute$9", "t2$lzycompute", "t2$lzycompute$1", "t3$lzycompute", "t3$lzycompute$1")
    assert(
      lzyComputeMethods == expComputeMethods,
      s"wrong lzycompute methods. expected:\n$expComputeMethods\nfound:\n$lzyComputeMethods")

    val fields = c.getClass.getDeclaredFields.toList.sortBy(_.getName).map(_.toString)
    val expFields = List(
      "private volatile byte C.bitmap$0",
      "private int C.lvl1",
      "private java.lang.String C.lvl2",
      "private scala.runtime.BoxedUnit C.lvl3",
      "private int C.t1",
      "private java.lang.String C.t2",
      "private scala.runtime.BoxedUnit C.t3",
      "private final int C.vl1",
      "private final java.lang.String C.vl2",
      "private final scala.runtime.BoxedUnit C.vl3",
      "private int C.vr1",
      "private java.lang.String C.vr2",
      "private scala.runtime.BoxedUnit C.vr3")
    assert(
      fields == expFields,
      s"wrong fields. expected:\n$expFields\nfound:\n$fields")


    val d = new D
    d.run()

    val dFields = d.getClass.getDeclaredFields.toList.sortBy(_.getName).map(_.toString)
    assert(
      dFields == expFields.map(_.replaceAll(" C.", " D.")),
      s"wrong fields. expected:\n$expFields\nfound:\n$fields")


    val d1 = new D1
    d1.run()

    val e = new E
    log(e.T); log(e.T)
    checkLog("init T:T:T")
    log(e.m); log(e.m)
    checkLog("init T:1:init T:1")
  }
}
