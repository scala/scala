package examples.pilib

import scala.concurrent.pilib._
//import pilib._;

/** Church encoding of naturals in the Pi-calculus */
object piNat extends Application {

  /** Locations of Pi-calculus natural */
  class NatChan extends Chan[Triple[Chan[unit], Chan[NatChan], Chan[NatChan]]]

  /** Zero */
  def Z(l: NatChan): unit = choice (
    l * { case Triple(z, sd, d) => z.write(()) }
  )

  /** Successor of Double */
  def SD(n: NatChan, l: NatChan): unit = choice (
    l * { case Triple(z, sd, d) => sd.write(n) }
  )

  /** Double */
  def D(n: NatChan, l: NatChan): unit = choice (
    l * { case Triple(z, sd, d) => d.write(n) }
  )

  /** Make "l" a location representing the natural "n" */
  def make(n: int, l: NatChan): unit =
    if (n == 0) Z(l)
    else if (n % 2 == 0) { val l1 = new NatChan; spawn < D(l1, l) >; make(n/2, l1) }
    else { val l1 = new NatChan; spawn < SD(l1, l) >; make(n/2, l1) }

  /** Consume the natural "m" and put it successor at location "n" */
  def Succ(m: NatChan, n: NatChan): unit = {
    val z = new Chan[unit]
    val sd = new Chan[NatChan]
    val d = new Chan[NatChan]
    spawn < m.write(Triple(z, sd, d)) >;
    choice (
      z  * { x => make(1, n) },
      sd * { m1 => { val n1 = new NatChan; spawn < D(n1, n) >; Succ(m1, n1) } },
      d  * { m1 => SD(m1, n) }
    )
  }

  /** Consume the natural "l" and put two copies at locations "m" and "n" */
  def Copy(l: NatChan, m: NatChan, n: NatChan): unit = {
    val z = new Chan[unit]
    val sd = new Chan[NatChan]
    val d = new Chan[NatChan]
    spawn < l.write(Triple(z, sd, d)) >;
    choice (
      z  * { x => spawn < Z(m) >; Z(n)  },
      sd * { l1 => { val m1 = new NatChan; val n1 = new NatChan;
                    spawn < SD(m1, m) | SD(n1, n) >;
                    Copy(l1, m1, n1) } },
      d * { l1 => { val m1 = new NatChan; val n1 = new NatChan;
                   spawn < D(m1, m) | D(n1, n) >;
                   Copy(l1, m1, n1) } }
    )
  }

  /** Consume the natural at location "n" and return its value */
  def value(n: NatChan): int =  {
    val z = new Chan[unit]
    val sd = new Chan[NatChan]
    val d = new Chan[NatChan]
    spawn < n.write(Triple(z, sd, d)) >;
    choice (
      z  * { x => 0  },
      sd * { n1 => 2 * value(n1) + 1 },
      d * { n1 => 2 * value(n1) }
    )
  }

  // Test
  val i = 42
  val l = new NatChan
  val l1 = new NatChan
  val l2 = new NatChan
  val l3 = new NatChan

  spawn <
  make(i, l) |
  Copy(l, l1, l2) |
  Succ(l2, l3) |
  System.out.println("" + i + " = " + value(l1)) |
  System.out.println("succ " + i + " = " + value(l3)) >

}
