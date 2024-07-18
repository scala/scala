//> using options -Xfatal-warnings -deprecation
//
class FooMapView extends collection.MapView[Int,Int] {
  def iterator: Iterator[(Int,Int)] = ???
  def get(key: Int) = None
  override def stringPrefix = "FooMapView"
}
