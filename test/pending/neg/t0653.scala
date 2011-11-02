// What is this test in place to test for?
//
class One[A]
class Two[A, B]
class Fix[Op[A]](x : Op[Fix[Op]])

class FixTest {
  // works
  // val zero = new Fix[One](new One)
  
  // don't work:
  val two = new Fix(new Two)    // this was what I found here
  val zero = new Fix(new One)   // this seems like something which could plausibly work
  
  // neg/t0653.scala:12: error: no type parameters for constructor Fix: (x: Op[Fix[Op[A]]])Fix[Op[A]] exist so that it can be applied to arguments (Two[Nothing,Nothing])
  //  --- because ---
  // argument expression's type is not compatible with formal parameter type;
  //  found   : Two[Nothing,Nothing]
  //  required: ?Op[ Fix[?Op[ A ]] ]
  //   val two = new Fix(new Two)    // this was what I found here
  //             ^
  // neg/t0653.scala:13: error: no type parameters for constructor Fix: (x: Op[Fix[Op[A]]])Fix[Op[A]] exist so that it can be applied to arguments (One[Nothing])
  //  --- because ---
  // argument expression's type is not compatible with formal parameter type;
  //  found   : One[Nothing]
  //  required: ?Op[ Fix[?Op[ A ]] ]
  //   val zero = new Fix(new One)   // this seems like something which could plausibly work  
  //              ^
  // two errors found    
}
