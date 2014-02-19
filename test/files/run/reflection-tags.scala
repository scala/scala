import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

object Test extends App {
  var typeMembers = typeOf[scala.reflect.api.Universe].members.filter(sym => sym.isType && !sym.isClass).toList
  typeMembers = typeMembers.filter(_.name != TypeName("ModifiersCreator")) // type ModifiersCreator = ModifiersExtractor
  typeMembers = typeMembers.filter(_.name != TypeName("Importer")) // deprecated
  typeMembers = typeMembers.filter(_.name != TypeName("Internal")) // internal
  typeMembers = typeMembers.filter(_.name != TypeName("Compat")) // internal
  typeMembers = typeMembers.filter(_.name != TypeName("BuildApi")) // deprecated
  val tags = typeOf[scala.reflect.api.Universe].members.filter(sym => sym.isImplicit).toList

  typeMembers.foreach(_.info)
  tags.foreach(_.info)

  val outliers = typeMembers.filter(tm => !tags.exists(tag => tag.info match {
    case NullaryMethodType(TypeRef(_, sym, targ :: Nil)) => sym == typeOf[ClassTag[_]].typeSymbol && targ.typeSymbol == tm
    case _ => false
  }))
  println(outliers)
}