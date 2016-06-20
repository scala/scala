class Global {
  class Typer[T]
}

class Reifier[T](global: Global)(typer: global.Typer[T])

object Test {
  def mkReifier[T](global: Global)(typer: global.Typer[T]) = typer
}