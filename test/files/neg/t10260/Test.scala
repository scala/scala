class IAImpl extends IA { def foo(a: A[_]) = ??? }
class IBImpl extends IB { def foo(a: B[_,_]) = ??? }
class ICImpl extends IC { def foo(a: Int, b: C[_], c: String) = ??? }
class IDImpl extends ID { def foo(a: D[_ <: String]) = ??? }
