abstract class Foo[@specialized T: ClassManifest, U <: Ordered[U]](x: T, size: Int) {
  var y: T
  var z: T = x

  def initialSize = 16
  val array = new Array[T](initialSize + size)

  def getZ = z
  def setZ(zz: T) = z = zz
}
