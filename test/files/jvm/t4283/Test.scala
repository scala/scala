
object Test extends App {
  val x = (new test.ScalaBipp).make.get.t // java.lang.IllegalAccessError: tried to access class test.AbstractFoo from class other.IllegalAccess$
}
