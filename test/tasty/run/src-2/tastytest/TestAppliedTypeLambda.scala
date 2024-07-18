package tastytest

object TestAppliedTypeLambda extends Suite("TestAppliedTypeLambda") {

  test("make empty array") {
    val arr: Array[Int] = AppliedTypeLambda.emptyArray[Int]
    assert(arr.getClass().isArray() && arr.getClass().getComponentType() == java.lang.Integer.TYPE)
  }

  test("make empty array with class Tparam") {
    val factory = new AppliedTypeLambda.InTypeParam[reflect.ClassTag]
    val arr: Array[Int] = factory.emptyArray[Int]
    assert(arr.getClass().isArray() && arr.getClass().getComponentType() == java.lang.Integer.TYPE)
  }

  test("make empty array with method Tparam") {
    val arr: Array[Int] = AppliedTypeLambda.emptyArray2[Int, reflect.ClassTag]
    assert(arr.getClass().isArray() && arr.getClass().getComponentType() == java.lang.Integer.TYPE)
  }
}


