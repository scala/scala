trait Y { type X }
trait W { def u[A](v : Y { type X = A }) : Unit }
class Z extends W { def u[A](v : Y { type X = A }) = null }

