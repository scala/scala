package tastytest

import scala.beans.BeanProperty

object InvisibleDefs {

  @main def argIsHello(arg: String): Unit = assert(arg == "Hello")

  class MyBean {

    @BeanProperty
    var status = ""

  }

}
