// scalac: -Werror -Wunused

//import language.existentials

trait OtherType[T]
case class Existensialism(field: OtherType[_])

class C {
  def f(clazz: Class[_]) = clazz.getSuperclass :: clazz.getInterfaces.toList
}
