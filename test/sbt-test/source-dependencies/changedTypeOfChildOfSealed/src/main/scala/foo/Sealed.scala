package foo

sealed trait Sealed

object Child1 extends Sealed with Base
object Child2 extends Sealed // 'with Base' will be added then removed

trait Base
trait OtherBase extends Base