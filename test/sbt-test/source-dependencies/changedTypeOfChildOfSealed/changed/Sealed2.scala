package foo

sealed trait Sealed

object Child1 extends Sealed with Base
object Child2 extends Sealed with OtherBase

trait Base
trait OtherBase