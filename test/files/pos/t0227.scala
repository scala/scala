final class Settings {
    def f[T](a_args: T*): List[T] = Nil
}

abstract class Factory {
    type libraryType <: Base

    final def apply(settings: Settings): libraryType = error("bla")
}

abstract class Base {
    val settings: Settings

    protected val demands: List[Factory] = Nil
}

class SA(val settings: Settings) extends Base {
    override val demands =  List(
            SD
        ) :::  settings.f(
            SC
        )    
}

object SC extends Factory {
    type libraryType = Base
}

object SD extends Factory {
    type libraryType = SA
}
