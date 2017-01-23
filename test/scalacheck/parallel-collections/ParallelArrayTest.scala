// package test.scala.collection.parallel.mutable

// import org.scalatest.FunSuite
// import collection.parallel.mutable.ParallelArray

// /**
//  * Notes:
//  */
// class ParallelArrayTest extends FunSuite {

//   test("create new parallel array with a bad initial capacity"){
//     intercept[IllegalArgumentException]{
//       new ParallelArray(-5)
//     }

//     /**
//      * this currently passes, but do we want it to?
//      * does it have meaning to have an empty parallel array?
//      */
//     new ParallelArray(0)
//     ()
//   }

//   test("compare identical ParallelArrays"){
//     assert(new ParallelArray(5) === new ParallelArray(5))
//     assert(ParallelArray(1,2,3,4,5) === ParallelArray(1,2,3,4,5))
//   }

//   /**
//    * this test needs attention. how is equality defined on ParallelArrays?
//    * Well, the same way it is for normal collections, I guess. For normal arrays its reference equality.
//    * I do not think it should be that way in the case of ParallelArray-s. I'll check this with Martin.
//    */
//   test("compare non-identical ParallelArrays"){
//     assert(ParallelArray(1,2,3,4,5) != ParallelArray(1,2,3,4),
//       "compared PA's that I expect to not be identical, but they were!")
//   }

//   test("creation via PA object [String]"){
//     val paFromApply: ParallelArray[String] = ParallelArray("x", "1", "true", "etrijwejiorwer")
//     val paFromHandoff: ParallelArray[String] = ParallelArray.handoff(Array("x", "1", "true", "etrijwejiorwer"))
//     val paFromCopy: ParallelArray[String] = ParallelArray.createFromCopy(Array("x", "1", "true", "etrijwejiorwer"))
//     assert( paFromApply === paFromCopy )
//     assert( paFromApply === paFromCopy )
//   }

// //  // handoffs dont work for primitive types...
// //  test("creation via PA object [Boolean]"){
// //    val paFromApply: ParallelArray[Boolean] = ParallelArray(true, false, true, false)
// //    val paFromCopy: ParallelArray[Boolean] = ParallelArray.createFromCopy(Array(true, false, true, false))
// //    assert( paFromApply === paFromCopy )
// //  }
// //
// //  // handoffs dont work for primitive types...
// //  test("creation via PA object [Int]"){
// //    val paFromApply: ParallelArray[Int] = ParallelArray(1, 2, 4, 3)
// //    val paFromCopy: ParallelArray[Int] = ParallelArray.createFromCopy(Array(1, 2, 4, 3))
// //    assert( paFromApply === paFromCopy )
// //  }

//   /**
//    * This fails because handoff is really doing a copy.
//    * TODO: look at handoff
//    */
//   test("Handoff Is Really A Handoff"){
//     val arrayToHandOff = Array("a", "x", "y", "z")
//     val paFromHandoff: ParallelArray[String] = ParallelArray.handoff(arrayToHandOff)
//     arrayToHandOff(0) = "w"
//     assert(paFromHandoff(0) === "w")
//   }

//   test("simple reduce"){
//     assert( ParallelArray(1,2,3,4,5).reduce(_+_) === 15 )
//   }

//   test("simple count"){
//     assert( ParallelArray[Int]().count(_ > 7) === 0 )
//     assert( ParallelArray(1,2,3).count(_ > 7) === 0 )
//     assert( ParallelArray(1,2,3).count(_ <= 3) === 3 )
//     assert( ParallelArray(1,2,3,4,5,6,7,8,9,10).count(_ > 7 ) === 3 )
//   }

//   test("simple forall"){
//     assert( ParallelArray[Int]().forall(_ > 7) === true )
//     assert( ParallelArray(1,2,3).forall(_ > 3) === false )
//     assert( ParallelArray(1,2,3).forall(_ <= 3) === true )
//     assert( ParallelArray(1,2,3,4,5,6,7,8,9,10).forall(_ > 0) === true )
//     assert( ParallelArray(1,2,3,4,5,6,7,8,9,10).forall(_ < 5) === false )
//   }

//   /**
//    */
//   test("simple foreach"){
//     val buf = new java.util.concurrent.ArrayBlockingQueue[Int](10000)
//     ParallelArray((1 to 10000):_*).foreach(buf add _)
//     (1 to 10000).foreach(i => assert( buf contains i, "buf should have contained:" + i ))
//   }

//   test("simple exists"){
//     assert( ParallelArray[Int]().exists(_ => true) === false )
//     assert( ParallelArray(1,2,3).forall(_ > 3) === false )
//     assert( ParallelArray(1,2,3,4,5,6,7,8,9,10).exists(_ > 7) === true )
//   }

//   test("simple filter"){
//     assert(ParallelArray(1,2,3,4,5).filter( _ < 4 ) === ParallelArray(1,2,3))
//   }

//   test("simple map test"){
//     assert(ParallelArray(1,2,3,4,5).map( (_:Int) * 10 ) === ParallelArray(10,20,30,40,50))
//   }
// }
