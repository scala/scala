package scala.reflect

import scala.runtime.ScalaRunTime._
import mirror._
import definitions._

object TagInterop {
  def arrayTagToClassManifest[T](tag: ArrayTag[T]): ClassManifest[T] = {
    val erasure = arrayElementClass(tag)
    if (erasure.isArray) {
      val elementClass = arrayElementClass(erasure)
      val elementManifest = arrayTagToClassManifest(ClassTag(elementClass))
      ClassManifest.arrayType(elementManifest).asInstanceOf[ClassManifest[T]]
    } else {
      ClassManifest.fromClass(erasure.asInstanceOf[Class[T]])
    }
  }

  def concreteTypeTagToManifest[T](tag: ConcreteTypeTag[T]): Manifest[T] = {
    // todo. reproduce manifest generation code here. toolboxes are too slow.
    val implicitly = PredefModule.typeSignature.member(newTermName("implicitly"))
    val taggedTpe = appliedType(staticClass("scala.reflect.Manifest").asTypeConstructor, List(tag.tpe))
    val materializer = TypeApply(Ident(implicitly), List(TypeTree(taggedTpe)))
    try mkToolBox().runExpr(materializer).asInstanceOf[Manifest[T]]
    catch { case ex: Throwable => Manifest.classType(tag.erasure).asInstanceOf[Manifest[T]] }
  }

  def manifestToConcreteTypeTag[T](tag: Manifest[T]): ConcreteTypeTag[T] = {
    val tpe =
      if (tag.typeArguments.isEmpty) classToType(tag.erasure)
      else appliedType(classToType(tag.erasure).typeConstructor, tag.typeArguments map (manifestToConcreteTypeTag(_)) map (_.tpe))
    ConcreteTypeTag(tpe, tag.erasure)
  }
}