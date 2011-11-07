class ClassCounts extends scala.collection.mutable.HashMap[Class[_], Int] { }

class A {  
  def f(xs: ClassCounts) {
    // ok
    xs(getClass) = xs(getClass) + 1
    // not ok
    xs(getClass) += 1
  }
}

// [Not checkable: parser]
// [Not checkable: namer]
// [Not checkable: packageobjects]
// [Now checking: typer]
// test/checker-tests/fail10.scala:8: error: 
// **** ERROR DURING INTERNAL CHECKING ****
// type mismatch;
//  found   : java.lang.Class[?0(in value ev$1)] where type ?0(in value ev$1)
//  required: java.lang.Class[?0(in method f)] where type ?0(in method f)
//     xs(getClass) += 1
//        ^
// one error found
