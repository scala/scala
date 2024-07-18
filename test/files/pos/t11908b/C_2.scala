//> using jvm 16+
object C {

  def useR0: Unit = {
    val r0 = new R0
  }

  def useR1 = {
    // constructor signature
    val r1 = new R1(123, "hello")

    // accessors signature
    val i: Int = r1.i
    val s: String = r1.s
    val j: Int = r1.i()

    // method
    val s2: String = r1.someMethod()

    // supertype
    val isRecord: java.lang.Record = r1

    ()
  }

  def useR2 = {
    // constructor signature
    val r2 = new R2.R(123, "hello")

    // accessors signature
    val i: Int = r2.i
    val s: String = r2.s

    // method
    val i2: Int = r2.getInt

    // supertype
    val isIntLike: IntLike = r2
    val isRecord: java.lang.Record = r2

    ()
  }

  def useR3 = {
    // constructor signature
    val r3 = new R3(123, 42L, "hi")
    new R3("hi", 123)

    // accessors signature
    val i: Int = r3.i
    val l: Long = r3.l
    val s: String = r3.s

    locally {
      val i: Int = r3.i()
      val l: Long = r3.l()
      val s: String = r3.s()
    }

    // method
    val l2: Long = r3.l(43L, 44L)

    // supertype
    val isRecord: java.lang.Record = r3
  }

  def useR4: Unit = {
    val r4 = new R4(42)
    val n: Int = r4.t
  }
}
