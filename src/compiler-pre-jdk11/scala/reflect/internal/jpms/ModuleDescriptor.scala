package scala.reflect.internal.jpms

// stubs for compiler-jdk11/scala/reflect/internal/jpms/JpmsModuleDescriptor.java
case class JpmsModuleDescriptor(name: String, directives: java.util.List[RequireDirective])
case class RequireDirective(moduleName: String, transitive: Boolean, isStatic: Boolean)
