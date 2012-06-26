object Test extends App {
  def cm1[T: ClassManifest] = ???
  def cm2[T](implicit evidence$1: ClassManifest[T]) = ???
  val cm3: ClassManifest[Int] = null

  def rcm1[T: scala.reflect.ClassManifest] = ???
  def rcm2[T](implicit evidence$1: scala.reflect.ClassManifest[T]) = ???
  val rcm3: scala.reflect.ClassManifest[Int] = null

  type CM[T] = ClassManifest[T]
  def acm1[T: CM] = ???
  def acm2[T](implicit evidence$1: CM[T]) = ???
  val acm3: CM[Int] = null

  type RCM[T] = scala.reflect.ClassManifest[T]
  def arcm1[T: RCM] = ???
  def arcm2[T](implicit evidence$1: RCM[T]) = ???
  val arcm3: RCM[Int] = null

  def m1[T: Manifest] = ???
  def m2[T](implicit evidence$1: Manifest[T]) = ???
  val m3: Manifest[Int] = null

  def rm1[T: scala.reflect.Manifest] = ???
  def rm2[T](implicit evidence$1: scala.reflect.Manifest[T]) = ???
  val rm3: scala.reflect.Manifest[Int] = null

  type M[T] = Manifest[T]
  def am1[T: M] = ???
  def am2[T](implicit evidence$1: M[T]) = ???
  val am3: M[Int] = null

  type RM[T] = scala.reflect.Manifest[T]
  def arm1[T: RM] = ???
  def arm2[T](implicit evidence$1: RM[T]) = ???
  val arm3: RM[Int] = null
}