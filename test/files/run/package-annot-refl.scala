package foo {

  import scala.annotation.StaticAnnotation
  class annot(val x: Any) extends StaticAnnotation

  @annot("hello world")
  package object bar {
    def one = 1
  }

}

object Test extends App {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{ currentMirror => mirror }

  // Package symbol
  val packageSym = mirror.staticPackage("foo.bar")
  // Package object symbol
  val poSym = mirror.staticModule("foo.bar.package")

  // Type of annotation
  val annotTpe = typeOf[foo.annot]

  // Check annotation is on package object
  assert(poSym.annotations match {
    case List(Annotation(`annotTpe`, _, _)) => true
    case _ => false
  })

  // Check annotation is on package
  assert(packageSym.annotations match {
    case List(Annotation(`annotTpe`, _, _)) => true
    case _ => false
  })

}
