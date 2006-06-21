package scala.actors.gui.event

case class MouseMoved(override val event: java.awt.event.MouseEvent) extends MouseEvent(event);
