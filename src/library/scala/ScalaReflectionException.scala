package scala

/** An exception that indicates an error during Scala reflection */
case class ScalaReflectionException(msg: String) extends Exception(msg)
