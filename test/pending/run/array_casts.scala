object Test {
   val a = Array(10)
   def main(args : Array[String]) : Unit = {
     val a = this.a : AnyRef
     Console.println("is object - " + a.isInstanceOf[Object])
     Console.println("is seq - " + a.isInstanceOf[Seq[_]])
     Console.println("is collection - " + a.isInstanceOf[Collection[_]])
     Console.println("is random-access-seq - " + a.isInstanceOf[RandomAccessSeq[_]])
     Console.println("is random-access-seq-mutable - " + a.isInstanceOf[RandomAccessSeq.Mutable[_]])
     Console.println("not string - " + !a.isInstanceOf[String])
     Console.println("not list - " + !a.isInstanceOf[List[_]])
     try {
       Console.println(a.asInstanceOf[Object].getClass)
     } catch { case ex : ClassCastException => Console.println("Bad, arrays should be objects") }
     try {
       Console.println(a.asInstanceOf[Seq[_]])
     } catch { case ex : ClassCastException => Console.println("Bad, arrays should be seqs") }
     try {
       Console.println(a.asInstanceOf[Collection[_]])
     } catch { case ex : ClassCastException => Console.println("Bad, arrays should be collections") }
     try {
       Console.println(a.asInstanceOf[RandomAccessSeq[_]])
     } catch { case ex : ClassCastException => Console.println("Bad, arrays should be random access seqs") }
     try {
       Console.println(a.asInstanceOf[RandomAccessSeq.Mutable[_]])
     } catch { case ex : ClassCastException => Console.println("Bad, arrays should be mutable random access seqs") }
     try { 
       Console.println("not expected: " + a.asInstanceOf[List[_]])
     } catch { case ex : ClassCastException => Console.println("Good, arrays are not lists") }
     try {
       Console.println("not expected: " + a.asInstanceOf[runtime.RichString])
       throw new Error("not expected")
     } catch { case ex : ClassCastException => Console.println("Good, arrays are not rich strings") }
     // check that arrays as seqs are still dynamically typed as arrays
     val s = this.a : Seq[Int]
     Console.println("is-seq array " + s.isInstanceOf[Array[Char]])
     try {
       Console.println(s.asInstanceOf[Array[Int]].getClass)
     } catch { case ex : ClassCastException => Console.println("Bad, arrays as seqs should still be arrays of int") }
     ()
   } 
}
