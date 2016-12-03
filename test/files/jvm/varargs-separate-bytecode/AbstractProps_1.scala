package foo

import scala.annotation.varargs

trait AbstractProps {
  @varargs
  def create(x: String, y: Int*): AbstractProps = null
}
