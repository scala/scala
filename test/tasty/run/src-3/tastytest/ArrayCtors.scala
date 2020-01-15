package tastytest

object ArrayCtors {

  object Module

  class ArrayCtor(val arr: Array[Module.type])

  object EmptyArrayCtor extends ArrayCtor(Array[Module.type]())

}
