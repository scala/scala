/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object heapsort {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);

      val numbers = new Array[Double](n+1);
      for (i <- Iterator.range(1,n+1)) 
         numbers(i) = generate(100.0);

      heapsort(n, numbers);

      Console.printf("{0,number,#.000000000}\n", numbers(n));
   }


   def heapsort(n: Int, ra: Array[Double]): Unit = {
      var l = 0; var j = 0; var ir = 0; var i = 0; 
      var rra = 0.0d;

      if (n < 2) return;
      l = (n >> 1) + 1;
      ir = n;
      while (true) {
         if (l > 1) { l = l-1; rra = ra(l); }
         else {
            rra = ra(ir);
            ra(ir) = ra(1);
            ir = ir-1;
            if (ir == 1) {
               ra(1) = rra;
               return;
            }
         }
         i = l;
         j = l << 1;
         while (j <= ir) {
            if (j < ir && ra(j) < ra(j+1)) { j = j+1; }
            if (rra < ra(j)) {
               ra(i) = ra(j);
               i = j;
               j = j + i;
            } 
            else j = ir + 1;
         }
         ra(i) = rra;
      }
   }


   private val IM = 139968;
   private val IA = 3877;
   private val IC = 29573;
   private var seed = 42;

   private def generate(max: Double) = {
      seed = (seed * IA + IC) % IM;
      max * seed / IM;
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }

}
