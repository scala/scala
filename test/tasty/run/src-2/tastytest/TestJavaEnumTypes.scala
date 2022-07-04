package tastytest

import java.lang.annotation.ElementType

object TestJavaEnumTypes extends Suite("TestJavaEnumTypes") {

  test(assert(JavaEnumTypes.method === ElementType.METHOD))
  test(assert(new JavaEnumTypes.TypeBox[ElementType.TYPE.type]().id(ElementType.TYPE) === ElementType.TYPE))
  test(assert(JavaEnumTypes.foo === 23))

  def compiletimeAsserts =
    forceAnnots[
      JavaEnumTypes.type,
      JavaEnumTypes.simulatedTarget,
      "new tastytest.JavaEnumTypes.simulatedTarget(Array.type.apply[java.lang.annotation.ElementType]((Array[java.lang.annotation.ElementType]{annotation#ElementType.type.LOCAL_VARIABLE}: java.lang.annotation.ElementType*))(reflect#ClassTag.type.apply[java.lang.annotation.ElementType](classOf[java.lang.annotation.ElementType])))"
    ]

}
