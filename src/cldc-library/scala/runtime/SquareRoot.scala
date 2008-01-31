/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $


package scala.runtime

/**
 * <p>
 *   Integer Square Root function (see http://atoms.alife.co.uk/sqrt/index.html).
 * </p>
 * <p>
 *   Contributors include Arne Steinarson for the basic approximation idea, Dann
 *   Corbit and Mathew Hendry for the first cut at the algorithm, Lawrence Kirby
 *   for the rearrangement, improvments and range optimization, Paul Hsieh
 *   for the round-then-adjust idea, Tim Tyler, for the Java port
 *   and Jeff Lawson for a bug-fix and some code to improve accuracy.
 * </p>
 *
 * @version v0.02 - 2003/09/07
 */

/**
 * Faster replacements for <code>(int)(java.lang.Math.sqrt(integer))</code>
 */
object SquareRoot {
  private val table = Array(
     0,    16,  22,  27,  32,  35,  39,  42,  45,  48,  50,  53,  55,  57,
     59,   61,  64,  65,  67,  69,  71,  73,  75,  76,  78,  80,  81,  83,
     84,   86,  87,  89,  90,  91,  93,  94,  96,  97,  98,  99, 101, 102,
     103, 104, 106, 107, 108, 109, 110, 112, 113, 114, 115, 116, 117, 118,
     119, 120, 121, 122, 123, 124, 125, 126, 128, 128, 129, 130, 131, 132,
     133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 144, 145,
     146, 147, 148, 149, 150, 150, 151, 152, 153, 154, 155, 155, 156, 157,
     158, 159, 160, 160, 161, 162, 163, 163, 164, 165, 166, 167, 167, 168,
     169, 170, 170, 171, 172, 173, 173, 174, 175, 176, 176, 177, 178, 178,
     179, 180, 181, 181, 182, 183, 183, 184, 185, 185, 186, 187, 187, 188,
     189, 189, 190, 191, 192, 192, 193, 193, 194, 195, 195, 196, 197, 197,
     198, 199, 199, 200, 201, 201, 202, 203, 203, 204, 204, 205, 206, 206,
     207, 208, 208, 209, 209, 210, 211, 211, 212, 212, 213, 214, 214, 215,
     215, 216, 217, 217, 218, 218, 219, 219, 220, 221, 221, 222, 222, 223,
     224, 224, 225, 225, 226, 226, 227, 227, 228, 229, 229, 230, 230, 231,
     231, 232, 232, 233, 234, 234, 235, 235, 236, 236, 237, 237, 238, 238,
     239, 240, 240, 241, 241, 242, 242, 243, 243, 244, 244, 245, 245, 246,
     246, 247, 247, 248, 248, 249, 249, 250, 250, 251, 251, 252, 252, 253,
     253, 254, 254, 255
  )

  /**
   * <p>
   *   A faster replacement for <code>(int)(java.lang.Math.sqrt(x))</code>.
   *   Completely accurate for <code>x &lt; 2147483648 (i.e. 2^31)</code>...
   * </p>
   * <p>
   *   Adjusted to more closely approximate "(int)(java.lang.Math.sqrt(x) + 0.5)"
   *   by Jeff Lawson.
   * </p>
   */
  @throws(classOf[IllegalArgumentException])
  def accurateSqrt(x: Int): Int = {
    if (x >= 0x10000) {
      val xn = if (x >= 0x1000000) {
        var xn0 =
          if (x >= 0x10000000)
            if (x >= 0x40000000) table(x >> 24) << 8
            else table(x >> 22) << 7
          else
            if (x >= 0x4000000) table(x >> 20) << 6
            else table(x >> 18) << 5

        xn0 = (xn0 + 1 + (x / xn0)) >> 1
        (xn0 + 1 + (x / xn0)) >> 1
      } else {
        var xn0 =
          if (x >= 0x100000)
            if (x >= 0x400000) table(x >> 16) << 4
            else table(x >> 14) << 3
          else
            if (x >= 0x40000) table(x >> 12) << 2
            else table(x >> 10) << 1

        (xn0 + 1 + (x / xn0)) >> 1
      }
      adjustment(x, xn)
    }
    else if (x >= 0x100) {
      val xn =
        if (x >= 0x1000)
          if (x >= 0x4000) (table(x >> 8)) + 1
          else (table(x >> 6) >> 1) + 1
        else
          if (x >= 0x400) (table(x >> 4) >> 2) + 1
          else (table(x >> 2) >> 3) + 1

      adjustment(x, xn)
    }
    else if (x >= 0) {
      adjustment(x, table(x) >> 4)
    }
    else {
      throw new IllegalArgumentException("Attempt to take the square root of negative number")
      -1
    }
  }

  private def adjustment(x: Int, xn: Int): Int = {
    // Added by Jeff Lawson:
    // need to test:
    //   if  |xn * xn - x|  >  |x - (xn-1) * (xn-1)|  then xn-1 is more accurate
    //   if  |xn * xn - x|  >  |(xn+1) * (xn+1) - x|  then xn+1 is more accurate
    // or, for all cases except x == 0:
    //    if  |xn * xn - x|  >  x - xn * xn + 2 * xn - 1 then xn-1 is more accurate
    //    if  |xn * xn - x|  >  xn * xn + 2 * xn + 1 - x then xn+1 is more accurate
    val xn2 = xn * xn

    // |xn * xn - x|
    var comparitor0 = xn2 - x
    if (comparitor0 < 0) comparitor0 = -comparitor0

    val twice_xn = xn << 1

    // |x - (xn-1) * (xn-1)|
    var comparitor1 = x - xn2 + twice_xn - 1
    if (comparitor1 < 0) comparitor1 = -comparitor1 // only gets here when x == 0

    // |(xn+1) * (xn+1) - x|
    val comparitor2 = xn2 + twice_xn + 1 - x

    if (comparitor0 > comparitor1)
      if (comparitor1 > comparitor2) xn+1 else xn-1
    else
      if (comparitor0 > comparitor2) xn+1 else xn
  }

  def main(args: Array[String]) {
    def toInt(s: String): Option[Int] =
      try { Some(s.toInt) } catch { case e: NumberFormatException => None }
    for (arg <- args; val x = toInt(arg); if !x.isEmpty) {
      val n = x.get
      println("sqrt("+n+") = "+accurateSqrt(n))
    }
  }
}
