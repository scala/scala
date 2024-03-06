package tastytest

import lib.InnerClassGen
import lib.InnerClass

// TODO [tasty]: create another test suite that tests the same source files but with standard compilation mode,
// i.e. Java symbols are read from classfiles produced by Javac compiler.
object TestInnerClass extends scala.App {

  // inner class generics
  locally {
    val ici = new InnerClassGen[Int]()

    val ici_inner: InnerClassGen[Int]#Inner[Long] = new ici.Inner[Long](23, 47L)

    val ici_inner2: InnerClassGen[Int]#Inner[Long] = ici.createInner[Long](23, 47L)

    val ici_inner3: InnerClassGen[Int]#Inner[Long] = InnerClassGen.createInnerStatic[Int, Long](23, 47L)

    val ic_staticInner: InnerClassGen.StaticInner[Long] = new InnerClassGen.StaticInner[Long](47L)
    val ic_staticInner2: InnerClassGen.StaticInner[Long] = InnerClassGen.createStaticInnerStatic[Long](47L)

    assert((ici_inner.outerField: Int) == 23)
    assert((ici_inner.innerField: Long) == 47L)

    assert((ici_inner.getOuterField: Int) == 23)
    assert((ici_inner.getInnerField: Long) == 47L)
  }

  // inner class
  locally {
    val ici = new InnerClass()

    val ici_inner: InnerClass#Inner[Long] = new ici.Inner[Long](47L)

    val ic_staticInner: InnerClass.StaticInner[Long] = new InnerClass.StaticInner[Long](47L)
    val ic_staticInner2: InnerClass.StaticInner[Long] = InnerClass.createStaticInnerStatic[Long](47L)

    /* see same issue for ici_inner3  */
    val ici_inner2: InnerClass#Inner[Long] = ici.createInner[Long](47L)

    /* TODO [tasty]: The TASTy produced for createInnerStatic is actually incorrect: see below
     *   43:       DEFDEF(41) 12 [createInnerStatic]
     *   46:         TYPEPARAM(9) 13 [U]
     *   49:           ...
     *   57:         PARAM(6) 15 [innerField]
     *   60:           ...
     *   65:         APPLIEDtpt(8)
     *   67:           SELECTtpt 16 [Inner] (this will fail without adaption because Inner is a non-static inner class)
     *   69:             SHAREDtype 30 (resolved to static value InnerClass.type)
     *   71:           IDENTtpt 13 [U]
     *   73:             SHAREDtype 62 ()
     * So either we do branching to lookup `Inner` in the non-static scope, or we do nothing
     * and fix the TASTy generation (AKA fix the scala 3 typer, see https://github.com/scala/scala3/issues/19619)
     */
    val ici_inner3: InnerClass#Inner[Long] = InnerClass.createInnerStatic[Long](47L)

    assert((ici_inner.innerField: Long) == 47L)

    assert((ici_inner.getInnerField: Long) == 47L)
  }

}
