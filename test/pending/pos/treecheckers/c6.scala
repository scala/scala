object Test6 {
  import scala.reflect.ClassTag
  def f[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass match { case x => x }
}
