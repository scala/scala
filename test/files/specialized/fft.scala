
/*
 * http://local.wasp.uwa.edu.au/~pbourke/miscellaneous/dft/
   Modification of Paul Bourkes FFT code by Peter Cusack 
   to utilise the Microsoft complex type.

   This computes an in-place complex-to-complex FFT 
   x and y are the real and imaginary arrays of 2^m points.
   dir =  1 gives forward transform
   dir = -1 gives reverse transform 
*/

import Math.{sqrt, pow}

/** Test that specialization handles tuples. Perform FFT transformation
 *  using pairs to represent complex numbers.
 */
object Test  {
  type Complex = (Double, Double)

  def swap(x: Array[Complex], i: Int, j: Int) {
    val tmp = x(i)
    x(i) = x(j)
    x(j) = tmp
  }

  def times(x: Complex, y: Complex): Complex = 
    (x._1 * y._1 - x._2 * y._2, x._1 * y._2 + x._2 * y._1)
    
  def div(x: Complex, y: Complex): Complex = {
    val num = pow(y._1, 2) + pow(y._2, 2)
    ((x._1 * y._1 + x._2 * y._2)/num,
     (x._2 * y._1 - x._1 * y._2)/num)
  }

  def div(x: Complex, y: Long) = 
    (x._1 / y, x._2 / y)

  def add(x: Complex, y: Complex) = 
    (x._1 + y._1, x._2 + y._2)

  def minus(x: Complex, y: Complex) =
    (x._1 - y._1, x._2 - y._2)

  def FFT(dir: Int, m: Long, x: Array[(Double, Double)]) {
    var i, i1, i2,j, k, l, l1, l2, n = 0l
//   complex <double> tx, t1, u, c;
    var tx, t1, u, c = (0.0, 0.0)

   /*Calculate the number of points */
   n = 1
   for (i <- 0l until m) 
      n <<= 1   

   /* Do the bit reversal */
   i2 = n >> 1
   j = 0

   for (i <- 0l until (n - 1)) {
      if (i < j)
         swap(x, i.toInt, j.toInt);

      k = i2;

      while (k <= j) {
         j -= k;
         k >>= 1;
      }

      j += k;
   }

   /* Compute the FFT */
   // c.real(-1.0);
   // c.imag(0.0);
   c = (-1.0, 0.0)
   l2 = 1
   for (l <- 0l until m) {
     l1 = l2
     l2 <<= 1;
      // u.real(1.0);
      // u.imag(0.0);
     u = (1.0, 0.0)

     for (j <- 0l until l1) {
       for (i <- j.until(n, l2)) {
         i1 = i + l1;
         t1 = times(u, x(i1.toInt))
         x(i1.toInt) = minus(x(i.toInt), t1) 
         x(i.toInt) = add(x(i.toInt), t1)
       }

       u = times(u,  c)
    }

     // c.imag(sqrt((1.0 - c.real()) / 2.0));
     c = (c._1, sqrt( (1.0 - c._1) / 2.0 ))
     // if (dir == 1)
     //    c.imag(-c.imag());
     if (dir == 1) 
       c = (c._1, -c._2)

      // c.real(sqrt((1.0 + c.real()) / 2.0));
      c = (sqrt( (1.0 + c._1) / 2.0), c._2)
   }

   /* Scaling for forward transform */
   if (dir == 1) {
     for (i <- 0l until n)
       x(i.toInt) = div(x(i.toInt), n)      
   }   
  }

  def run() {
    FFT(1, 16, data)
  }
  var data: Array[Complex] = null

  def inputFileName = {
    val cwd = System.getProperty("partest.cwd")
    if (cwd ne null) cwd + java.io.File.separator + "input2.txt"
    else "input2.txt"
  }

  def setUp {
//    print("Loading from %s.. ".format(inputFileName))
    val f = io.Source.fromFile(inputFileName)
    val lines = f.getLines
    val n = lines.next.toInt
    data = new Array[Complex](n)
    var i = 0
    for (line <- lines if line != "") {
      val pair = line.trim.split(" ")
      data(i) = (pair(0).trim.toDouble, pair(1).trim.toDouble)
      i += 1
    }
//    println("[loaded]")
    println("Processing " + n + " items")
  }

  def main(args: Array[String]) {
    setUp
    run()

    println("Boxed doubles: " + runtime.BoxesRunTime.doubleBoxCount)
    println("Boxed ints: " + runtime.BoxesRunTime.integerBoxCount)
    println("Boxed longs: " + runtime.BoxesRunTime.longBoxCount)
  }
}
