package tastytest.issue12420

class Boxxy[I <: Int, B <: Boxxy[I, B]]

object Boxxy {
  object default extends Boxxy[0, default.type]
}

class Qux[I <: Int, B <: Boxxy[I, B]](val inner: B)
class Eta extends Qux(Boxxy.default)
