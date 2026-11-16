import scala.reflect.runtime.universe._

// Enclosing's parent mentions Enclosing's member class. `isRawIfWithoutArgs` runs on the parent while Enclosing is working
// out its parents, so it must only load the parent: it's Enclosing's completion that completes the rest later. Testing
// `isJavaDefined` first initializes it there, which completes Enclosing again from within, and each pass enters the
// members. On JDK 21 and later the same order fails the universe's own initialization, on `MemberName$Factory`.
object Test extends App {
  val mirror = runtimeMirror(getClass.getClassLoader)
  println(mirror.classSymbol(classOf[p.Enclosing]).info.decls.toList)
  println(mirror.classSymbol(classOf[p.Parent]).info.decls.toList)
}
