object Test {

 def benchmarkA(num: Int) {

   type A = Int

   def updateM[M[_]](ms: M[A], update: (M[A], A)=>M[A]): M[A] = {
     var is = ms
     for (i <- 0 until num) is = update(is, i)
     is
   }

   //
   def vectorAppend: Vector[A] = updateM[Vector](Vector(), (as, a)=>{
     val v = (as :+ a)
     //println("==>append:    i: "+i1+", v: "+v)
     v
   })
   // this will crash, Vector bug!
   def vectorRemove(vec: Vector[A]): Vector[A] = updateM[Vector](vec, (as, a)=>{
     val v = (as filterNot{ _ == a})
     //val v = (is filter{ _ != i})
     //println("==>remove:    i: "+a)
     v
   })

   val ct = vectorAppend
   println(" append [num: "+num+"] vec")
   vectorRemove(ct)
   println(" remove [num: "+num+"] vec")
 } // BenchmarkA

 def comparison(num: Int): Unit = {
   for (i <- 1 until 5) benchmarkA(num*i)
   println(">> comparison done, num: "+num);
 }

 def main(args: Array[String]): Unit = {
   try {
     //createBenchmarkA(23).testRun

     comparison(200) // OK
     comparison(2000) // this will crach

   } catch {
     case e: Exception => e.printStackTrace()
   }
 }
}
