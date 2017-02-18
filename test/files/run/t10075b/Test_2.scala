class C {
  @(RetainedAnnotation @annotation.meta.field)
  lazy val lzyValFieldAnnotation = 42

  @(RetainedAnnotation @annotation.meta.getter)
  lazy val lzyValGetterAnnotation = 42

  @(RetainedAnnotation @annotation.meta.field)
  val valFieldAnnotation = 42

  @(RetainedAnnotation @annotation.meta.getter)
  val valGetterAnnotation = 42

  @(RetainedAnnotation @annotation.meta.field)
  var varFieldAnnotation = 42

  @(RetainedAnnotation @annotation.meta.getter)
  var varGetterAnnotation = 42

  @(RetainedAnnotation @annotation.meta.setter)
  var varSetterAnnotation = 42
}

trait T {
  @(RetainedAnnotation @annotation.meta.field)
  lazy val lzyValFieldAnnotation = 42

  @(RetainedAnnotation @annotation.meta.getter)
  lazy val lzyValGetterAnnotation = 42

  @(RetainedAnnotation @annotation.meta.field)
  val valFieldAnnotation = 42

  @(RetainedAnnotation @annotation.meta.getter)
  val valGetterAnnotation = 42

  @(RetainedAnnotation @annotation.meta.field)
  var varFieldAnnotation = 42

  @(RetainedAnnotation @annotation.meta.getter)
  var varGetterAnnotation = 42

  @(RetainedAnnotation @annotation.meta.setter)
  var varSetterAnnotation = 42

  @RetainedAnnotation
  def method = 42
}
class TMix extends T

object Test extends App {
  (List(classOf[C], classOf[T], classOf[TMix]).
    flatMap(cls => cls.getDeclaredFields ++ cls.getDeclaredMethods)).
    sortBy(x => (x.getDeclaringClass.getName, x.getName, x.toString)).
    foreach(x => println(x.getAnnotations.toList.mkString(" ") + " " + x))
}