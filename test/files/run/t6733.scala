import scala.reflect.runtime.universe._

trait Foo {
  private[this] val pri1a = 0
  // private[this] val pri1b: Int
  private[this] def pri2a = 1
  // private[this] def pri2b: Int
  private[this] var pri3a = 0
  // private[this] var pri3b: Int
  private[this] lazy val pri4a = 0
  // private[this] lazy val pri4b: Int
  private[this] type Pri5a = Int
  // private[this] type Pri5b <: Int
  private[this] class Pri6
  private[this] trait Pri7
  private[this] object Pri8

  protected[this] val pro1a = 0
  protected[this] val pro1b: Int
  protected[this] def pro2a = 1
  protected[this] def pro2b: Int
  protected[this] var pro3a = 0
  protected[this] var pro3b: Int
  protected[this] lazy val pro4a = 0
  // protected[this] lazy val pro4b: Int
  protected[this] type Pro5a = Int
  protected[this] type Pro5b <: Int
  protected[this] class Pro6
  protected[this] trait Pro7
  protected[this] object Pro8
}

object Test extends App {
  typeOf[Foo].decls.sorted.foreach(m => println(s"$m: isPrivateThis = ${m.isPrivateThis}, isProtectedThis = ${m.isProtectedThis}"))
}