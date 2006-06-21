package scala.actors.gui.event

case class MouseDragged(override val event: java.awt.event.MouseEvent) extends MouseEvent(event);
