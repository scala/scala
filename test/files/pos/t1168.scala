object Test extends App {
  
  trait SpecialException {}

  try {
    throw new Exception
  } catch {
    case e : SpecialException => {
      println("matched SpecialException: "+e)
      assume(e.isInstanceOf[SpecialException])
    }
    case e : Exception => {
      assume(e.isInstanceOf[Exception])
    }
  }
}
// t1168.scala:8:
//     case e : SpecialException => {
//              ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Throwable
//         pt  Throwable
//      pattp  Test.SpecialException
//   pattp+pt  Test.SpecialException with Throwable
//   pt+pattp  Throwable with Test.SpecialException
//     result  Throwable with Test.SpecialException
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }