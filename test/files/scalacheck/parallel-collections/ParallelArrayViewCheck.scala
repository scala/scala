// package scala.collection.parallel
// package mutable






// import org.scalacheck._
// import org.scalacheck.Gen
// import org.scalacheck.Gen._
// import org.scalacheck.Prop._
// import org.scalacheck.Properties
// import org.scalacheck.Arbitrary._

// import scala.collection.TraversableView
// import scala.collection.mutable.ArrayBuffer
// import scala.collection.parallel.ops._
// import scala.collection.mutable.ArraySeq



// abstract class ParallelArrayViewCheck[T](tp: String)
// extends ParallelSeqCheck[T]("ParallelSeqView[" + tp + ", ParallelArray[" + tp + "]]") {
//   // ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
//   // ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)
  
//   type CollType = ParallelSeqView[T, ParallelArray[T], ArraySeq[T]]
  
//   def isCheckingViews = true
  
//   def instances(vals: Seq[Gen[T]]): Gen[Seq[T]] = sized { sz =>
//     val a = new ArrayBuffer[T](sz)
//     val gen = vals(rnd.nextInt(vals.size))
//     for (i <- 0 until sz) a += sample(gen)
//     a
//   }
  
//   def fromSeq(a: Seq[T]) = {
//     val pa = new ParallelArray[T](a.size)
//     var i = 0
//     for (elem <- a) {
//       pa(i) = elem
//       i += 1
//     }
//     pa.view
//   }
  
//   property("forces must be equal") = forAll(collectionPairs) { case (s, coll) =>
//     val smodif = (s ++ s).reverse.take(s.length).reverse.zip(s).drop(s.length / 2)
//     val cmodif = (coll ++ s).reverse.take(s.length).reverse.zip(s).drop(s.length / 2).force
//     smodif == cmodif
//   }
  
// }


// object IntParallelArrayViewCheck extends ParallelArrayViewCheck[Int]("Int") with IntSeqOperators with IntValues {
//   override def instances(vals: Seq[Gen[Int]]) = oneOf(super.instances(vals), sized { sz =>
//     (0 until sz).toArray.toSeq
//   }, sized { sz =>
//     (-sz until 0).toArray.toSeq
//   })
// }


// abstract class ParallelArrayViewComposedCheck[T](tp: String)
// extends ParallelSeqCheck[T]("ParallelSeqView[" + tp + "], ParallelArray[" + tp + "].++.patch.reverse.take.reverse") {
//   ForkJoinTasks.defaultForkJoinPool.setMaximumPoolSize(Runtime.getRuntime.availableProcessors * 2)
//   ForkJoinTasks.defaultForkJoinPool.setParallelism(Runtime.getRuntime.availableProcessors * 2)
  
//   type CollType = collection.parallel.ParallelSeq[T]
  
//   def isCheckingViews = true
  
//   def instances(vals: Seq[Gen[T]]): Gen[Seq[T]] = sized { sz =>
//     val a = new ArrayBuffer[T](sz)
//     val gen = vals(rnd.nextInt(vals.size))
//     for (i <- 0 until sz) a += sample(gen)
//     a
//   }
  
//   def fromSeq(a: Seq[T]) = {
//     val pa = new ParallelArray[T](a.size)
//     var i = 0
//     for (elem <- a) {
//       pa(i) = elem
//       i += 1
//     }
//     val modified = (pa.view ++ a).patch(0, a, a.length).reverse
//     val original = modified.take(modified.length / 2).reverse
//     original
//   }
  
// }


// object IntParallelArrayViewComposedCheck extends ParallelArrayViewComposedCheck[Int]("Int") with IntSeqOperators with IntValues {
//   override def instances(vals: Seq[Gen[Int]]) = oneOf(super.instances(vals), sized { sz =>
//     (0 until sz).toArray.toSeq
//   }, sized { sz =>
//     (-sz until 0).toArray.toSeq
//   })
// }


















