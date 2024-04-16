//> using options -Xlint:serial -Xfatal-warnings
//
@SerialVersionUID(1L)
class X

@SerialVersionUID(1L)
class Y extends X

@SerialVersionUID(1L)
class Z extends scala.Serializable

@SerialVersionUID(1L)
class W extends java.io.Serializable

@SerialVersionUID(1L)
class Q extends Z

@SerialVersionUID(1L)
trait T

@SerialVersionUID(1L)
trait U extends scala.Serializable

@SerialVersionUID(1L)
trait V extends java.io.Serializable
