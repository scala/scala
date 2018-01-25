trait A
trait B extends A
trait C extends B
object it extends C

/* try as many weird diamondy things as I can think of */
trait SAM_A                 { def apply(): A }
trait SAM_A1 extends SAM_A  { def apply(): A }
trait SAM_B  extends SAM_A1 { def apply(): B }
trait SAM_B1 extends SAM_A1 { def apply(): B }
trait SAM_B2 extends SAM_B with SAM_B1
trait SAM_C  extends SAM_B2 { def apply(): C }

trait SAM_F  extends (() => A) with SAM_C
trait SAM_F1 extends (() => C) with SAM_F


object Test extends App {

  val s1: SAM_A  = () => it
  val s2: SAM_A1 = () => it
  val s3: SAM_B  = () => it
  val s4: SAM_B1 = () => it
  val s5: SAM_B2 = () => it
  val s6: SAM_C  = () => it
  val s7: SAM_F  = () => it
  val s8: SAM_F1 = () => it

  (s1(): A)

  (s2(): A)

  (s3(): B)
  (s3(): A)

  (s4(): B)
  (s4(): A)

  (s5(): B)
  (s5(): A)

  (s6(): C)
  (s6(): B)
  (s6(): A)

  (s7(): C)
  (s7(): B)
  (s7(): A)

  (s8(): C)
  (s8(): B)
  (s8(): A)

}
