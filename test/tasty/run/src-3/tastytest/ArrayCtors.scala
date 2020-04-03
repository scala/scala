package tastytest

object ArrayCtors {

  final class arrayAnnot(val arr: Array[Module.type]) extends scala.annotation.StaticAnnotation
  final class arrayAnnot2(val arr: Array[Array[Module.type]]) extends scala.annotation.StaticAnnotation

  object Module

  @arrayAnnot(Array[Module.type]())
  object EmptyArrayCtor

  @arrayAnnot2(Array[Array[Module.type]]())
  object EmptyArrayCtor2

}
