object Test {
  final val clasz = classOf[String]
  final val str = "Str"

  import java.lang.annotation.RetentionPolicy.CLASS.{ordinal => ord} // error: stable identifier required, but CLASS found
  println(ord)

  import clasz.{getName => claszName} // error: stable identifier required, but classOf[java.lang.String] found
  println(claszName)

  import str.{length => strLen} // error: stable identifier required, but "Str" found
  println(strLen)
}
