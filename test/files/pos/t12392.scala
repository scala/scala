//> using options -Werror
import scala.reflect.api.Universe

object Test {
  type SingletonUniverse = Universe with Singleton
  def deepIntersectionTypeMembers[U <: SingletonUniverse](targetType: U#Type): List[U#Type] = {
    def go(tpe: U#Type): List[U#Type] = {
      tpe match {
        case r: U#RefinedTypeApi => r.parents.flatMap(t => deepIntersectionTypeMembers[U]((t.dealias): U#Type))
        case _ => List(tpe)
      }
    }
    go(targetType).distinct
  }
}
