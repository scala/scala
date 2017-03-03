class reallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongname {
object obj0
object obj01
object obj012
object obj0123
object obj01234
object obj012345
object obj0123456
object obj01234567
object obj012345678
object obj0123456789
object obj01234567890
class cls0
class cls01
class cls012
class cls0123
class cls01234
class cls012345
class cls0123456
class cls01234567
class cls012345678
class cls0123456789
class cls01234567890
trait trt0
trait trt01
trait trt012
trait trt0123
trait trt01234
trait trt012345
trait trt0123456
trait trt01234567
trait trt012345678
trait trt0123456789
trait trt01234567890
}

object Test extends App {
  def check(c: Class[_]) {
    checkClassName(c.getName)
  }
  def checkClassName(name: String) {
    val defaultMaxClassFileLength = 255
    assert((name + ".class").length <= defaultMaxClassFileLength, name)
  }

  val c = new reallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongname
  import c._

  check(obj0.getClass)
  check(obj01.getClass)
  check(obj012.getClass)
  check(obj0123.getClass)
  check(obj01234.getClass)
  check(obj012345.getClass)
  check(obj0123456.getClass)
  check(obj01234567.getClass)
  check(obj012345678.getClass)
  check(obj0123456789.getClass)
  check(obj01234567890.getClass)

  check(classOf[cls0])
  check(classOf[cls01])
  check(classOf[cls012])
  check(classOf[cls0123])
  check(classOf[cls01234])
  check(classOf[cls012345])
  check(classOf[cls0123456])
  check(classOf[cls01234567])
  check(classOf[cls012345678])
  check(classOf[cls0123456789])
  check(classOf[cls01234567890])

  check(classOf[trt0])
  check(classOf[trt01])
  check(classOf[trt012])
  check(classOf[trt0123])
  check(classOf[trt01234])
  check(classOf[trt012345])
  check(classOf[trt0123456])
  check(classOf[trt01234567])
  check(classOf[trt012345678])
  check(classOf[trt0123456789])
  check(classOf[trt01234567890])

}

// filename too long: reallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongnamereallylongname$obj012345$.class
