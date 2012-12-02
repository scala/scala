import scala.reflect.ClassTag

trait ru {
	type Type
	type TypeRef <: Type
	object TypeRef {
		def unapply(t: TypeRef) = Some((0, 0, 0))
	}
	implicit def TypeRefTag: ClassTag[TypeRef] = implicitly
}

object ru extends ru

class C {
  def typeArguments(t: ru.Type)  {
    import ru._ // otherwise the pattern match will be unchecked
                // because TypeRef is an abstract type
    t match { case ru.TypeRef(_, _, _) => ; case _ => }
  }
}
