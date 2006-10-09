package scala.actors

trait OutputChannel[Msg] {
  def !(msg: Msg): Unit
  def forward(msg: Msg): Unit
}
