// if this file compiles, then we're good
package c

class foo(x: Any) extends annotation.StaticAnnotation

@foo(new AnyRef { }) trait A
