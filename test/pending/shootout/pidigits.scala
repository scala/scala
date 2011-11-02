/* ------------------------------------------------------------------ */
/* The Computer Language Shootout                               */
/* http://shootout.alioth.debian.org/                                 */
/*                                                                    */
/* Contributed by Anthony Borla                                       */
/* ------------------------------------------------------------------ */

object pidigits
{
  def main(args: Array[String]): Unit =
  {
    val N: Int = Integer.parseInt(args(0)); var i: Int = 10

    while (i <= N)
    {
      System.out.println(pi_digits(10) + "\t:" + i)
      i = i + 10
    }

    i = i - 10

    if (i < N)
    {
      System.out.println(pi_digits(N - i) + "\t:" + N)
    }
  }

  def compose(a: Array[BigInt], b: Array[BigInt]): Array[BigInt] =
  {
    return Array(a(0) * b(0),
                 a(0) * b(1) + a(1) * b(3),
                 a(2) * b(0) + a(3) * b(2),
                 a(2) * b(1) + a(3) * b(3))
  }

  def extract(a: Array[BigInt], j: Int): BigInt =
  {
    return (a(0) * j + a(1)) / (a(2) * j + a(3))
  }

  def pi_digits(c: Int): String =
  {
    val r: StringBuffer = new StringBuffer(); var i: Int = 0

    while (i < c)
    {
      var y: BigInt = extract(Z, 3)

      while (y != extract(Z, 4))
      {
        K = K + 1; Z = compose(Z, Array(K, 4 * K + 2, 0, 2 * K + 1))
        y = extract(Z, 3)
      }

//      Z = compose(Array(10, (-y) * 10, 0, 1), Z)

      Z = compose(Array(10, y * (-10), 0, 1), Z)

      r.append(y); i = i + 1; 
    }

    return r.toString()
  }

  var K: Int = 0

  var Z: Array[BigInt] = Array(1, 0, 0, 1)
}

