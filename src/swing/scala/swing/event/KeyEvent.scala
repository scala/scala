package scala.swing.event

import javax.swing.JComponent

sealed abstract class KeyEvent extends InputEvent {
  def peer: java.awt.event.KeyEvent
}

case class KeyTyped(val source: Component, char: Char, val modifiers: Int,
                    location: Key.Location.Value)
                   (val peer: java.awt.event.KeyEvent) extends KeyEvent {
  def this(e: java.awt.event.KeyEvent) = this(UIElement.cachedWrapper(e.getSource.asInstanceOf[JComponent]),
                               e.getKeyChar, e.getModifiersEx,
                               Key.Location(e.getKeyLocation))(e)
}

case class KeyPressed(val source: Component, key: Key.Value, val modifiers: Int,
                    location: Key.Location.Value)
                   (val peer: java.awt.event.KeyEvent) extends KeyEvent {
  def this(e: java.awt.event.KeyEvent) = this(UIElement.cachedWrapper(e.getSource.asInstanceOf[JComponent]),
                               Key(e.getKeyCode), e.getModifiersEx, Key.Location(e.getKeyLocation))(e)
}

case class KeyReleased(val source: Component, key: Key.Value, val modifiers: Int,
                    location: Key.Location.Value)
                   (val peer: java.awt.event.KeyEvent) extends KeyEvent {
  def this(e: java.awt.event.KeyEvent) = this(UIElement.cachedWrapper(e.getSource.asInstanceOf[JComponent]),
                               Key(e.getKeyCode), e.getModifiersEx, Key.Location(e.getKeyLocation))(e)
}
