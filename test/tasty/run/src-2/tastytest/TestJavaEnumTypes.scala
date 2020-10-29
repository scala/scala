package tastytest

import java.lang.annotation.ElementType

object TestJavaEnumTypes extends Suite("TestJavaEnumTypes") {

  test(assert(JavaEnumTypes.method === ElementType.METHOD))
  test(assert(new JavaEnumTypes.TypeBox[ElementType.TYPE.type]().id(ElementType.TYPE) === ElementType.TYPE))
  test(assert(JavaEnumTypes.foo === 23))

}
