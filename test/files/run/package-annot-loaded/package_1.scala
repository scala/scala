package foo {

  import scala.annotation.StaticAnnotation
  class annot extends StaticAnnotation

  @annot
  package object bar {
    def one = 1
  }

}
