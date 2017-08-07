object Test extends App {

  val one: java.lang.Integer = 1
  println(one.isInstanceOf[java.lang.Integer])
  println(one.asInstanceOf[java.lang.Integer])
  println(one.isInstanceOf[Number])
  println(one.asInstanceOf[Number].longValue())
  println(one.isInstanceOf[Comparable[_]])
  println(one.asInstanceOf[Comparable[java.lang.Integer]].compareTo(5))
  println(one.isInstanceOf[Object])
  println(one.asInstanceOf[Object].toString)
  println(one.isInstanceOf[java.io.Serializable])
  println(one.isInstanceOf[java.lang.Long])
  println(one.isInstanceOf[java.lang.Short])
  println(one.isInstanceOf[java.lang.Double])
  println(one.isInstanceOf[java.lang.Boolean])
  println(one.isInstanceOf[java.lang.Character])

  println()

  val two: java.lang.Long = 2L
  println(two.isInstanceOf[java.lang.Long])
  println(two.asInstanceOf[java.lang.Long])
  println(two.isInstanceOf[Number])
  println(two.asInstanceOf[Number].longValue())
  println(two.isInstanceOf[Comparable[_]])
  println(two.asInstanceOf[Comparable[java.lang.Long]].compareTo(5L))
  println(two.isInstanceOf[Object])
  println(two.asInstanceOf[Object].toString)
  println(two.isInstanceOf[java.io.Serializable])
  println(two.isInstanceOf[java.lang.Integer])
  println(two.isInstanceOf[java.lang.Double])
  println(two.isInstanceOf[java.lang.Boolean])
  println(two.isInstanceOf[java.lang.Character])

  println()

  val tru: java.lang.Boolean = true
  println(tru.isInstanceOf[java.lang.Boolean])
  println(tru.asInstanceOf[java.lang.Boolean])
  println(tru.isInstanceOf[Number])
  println(tru.isInstanceOf[Comparable[_]])
  println(tru.asInstanceOf[Comparable[java.lang.Boolean]].compareTo(false))
  println(tru.isInstanceOf[Object])
  println(tru.asInstanceOf[Object].toString)
  println(tru.isInstanceOf[java.io.Serializable])
  println(tru.isInstanceOf[java.lang.Integer])
  println(tru.isInstanceOf[java.lang.Double])
  println(tru.isInstanceOf[java.lang.Character])

  println()

  val tsu: java.lang.Character = 'つ'
  println(tsu.isInstanceOf[java.lang.Character])
  println(tsu.asInstanceOf[java.lang.Character])
  println(tsu.isInstanceOf[Number])
  println(tsu.isInstanceOf[Comparable[_]])
  println(tsu.asInstanceOf[Comparable[java.lang.Character]].compareTo('ツ') < 0)
  println(tsu.isInstanceOf[Object])
  println(tsu.asInstanceOf[Object].toString)
  println(tsu.isInstanceOf[java.io.Serializable])
  println(tsu.isInstanceOf[java.lang.Integer])
  println(tsu.isInstanceOf[java.lang.Double])
  println(tsu.isInstanceOf[java.lang.Boolean])

}