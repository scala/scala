package scala.reflect.runtime

import java.lang.reflect.GenericDeclaration

trait JavaConversions { self: Universe =>

  def jOwnerChain(jclazz: java.lang.Class[_]): List[GenericDeclaration] = {
    {
      val owner = jclazz.getEnclosingConstructor
      if (owner != null) return owner :: jOwnerChain(owner)
    };{
      val owner = jclazz.getEnclosingMethod
      if (owner != null) return owner :: jOwnerChain(owner)
    };{
      val owner = jclazz.getEnclosingClass
      if (owner != null) owner :: jOwnerChain(owner) else List()
    }
  }

  def jOwnerChain(jconstr: java.lang.reflect.Method): List[GenericDeclaration] = {
    val owner = jconstr.getDeclaringClass
    owner :: jOwnerChain(owner)
  }

  def jOwnerChain(jconstr: java.lang.reflect.Constructor[_]): List[GenericDeclaration] = {
    val owner = jconstr.getDeclaringClass
    owner :: jOwnerChain(owner)
  }

  def toplevelScalaClass(jclazz: java.lang.Class[_]): ClassSymbol = {
    println(jclazz.getAnnotations().toList map (_.annotationType))
    null
  }

  def navigate(root: Symbol, path: List[GenericDeclaration]): Symbol = NoSymbol

  def scalaClass(jclazz: java.lang.Class[_]): Symbol = {
    val path = (jclazz :: jOwnerChain(jclazz)).reverse
    val topClass = toplevelScalaClass(path.head.asInstanceOf[java.lang.Class[_]])
    navigate(topClass, path)
  }
}