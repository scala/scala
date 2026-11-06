class self(x: Int = (1: @self)) extends scala.annotation.StaticAnnotation

class a(x: Int = (1: @b)) extends scala.annotation.StaticAnnotation
class b(x: Int = (1: @a)) extends scala.annotation.StaticAnnotation
