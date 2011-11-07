class T[A[Y] <: T[A, B], B]
class T2[X] extends T[T2, X] // ill-typed
// because this bound is not met:
// Forall Y. T2[Y] <: T[T2, X]

// debugging before fix:
// def isSubType0 -->       
// case (PolyType(tparams1, res1), PolyType(tparams2, res2)) => println("<:<PT: "+((tparams1, res1), (tparams2, res2))) //@MDEBUG
//   (tparams1.length == tparams2.length &&
//    List.forall2(tparams1, tparams2) 
//      ((p1, p2) => p2.info.substSym(tparams2, tparams1) <:< p1.info) &&
//    res1 <:< res2.substSym(tparams2, tparams1))

// generates output:
// <:<PT: ((List(type Y),A[Y]),(List(type Y),T[A,B]))
// <:<PT: ((List(type X),T2[X]),(List(type Y),T[T2,X]))

// however, last check should have been:
// <:<PT: ((List(type X1),T2[X1]),(List(type Y1),T[T2,X]))

// case (PolyType(tparams1, res1), PolyType(tparams2, res2)) => println("<:<PT: "+((tparams1, res1), (tparams2, res2))) //@MDEBUG
//   (tparams1.length == tparams2.length &&
//    {
//      val tpsFresh = cloneSymbols(tparams1) // @M cloneSymbols(tparams2) should be equivalent -- TODO: check
//      List.forall2(tparams1, tparams2) 
//         ((p1, p2) => p2.info.substSym(tparams2, tpsFresh) <:< p1.info.substSym(tparams1, tpsFresh)) &&
//       res1.substSym(tparams1, tpsFresh) <:< res2.substSym(tparams2, tpsFresh)   
//    })
