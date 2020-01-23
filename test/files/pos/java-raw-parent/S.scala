package java_raw_parent

object Test { C.test(new C) }
// S.scala:3: error: type mismatch;
//  found   : java_raw_parent.C
//  required: java_raw_parent.A[_]
// Note: java_raw_parent.B <: Any (and java_raw_parent.C <: java_raw_parent.A[java_raw_parent.B]), but Java-defined trait A is invariant in type T.
// You may wish to investigate a wildcard type such as `_ <: Any`. (SLS 3.2.10)
// object Test { C.test(new C) }
//                      ^
