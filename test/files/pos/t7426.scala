class foo(x: Any) extends annotation.StaticAnnotation

@foo(new AnyRef { }) trait A
