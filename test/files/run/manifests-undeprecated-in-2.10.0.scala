object Test extends App {
  def m1a: scala.reflect.Manifest[Int] = scala.reflect.Manifest.Int
  def m2a: scala.reflect.OptManifest[Int] = ???
  def m3a = scala.reflect.NoManifest

  def m1b: Manifest[Int] = Manifest.Int
  def m2b: OptManifest[Int] = ???
  def m3b = NoManifest

  val m4a = manifest[Int]
  val m5a = optManifest[Int]

  val m4b = implicitly[Manifest[Int]]
  val m5b = implicitly[OptManifest[Int]]
}