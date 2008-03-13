package swing;

import javax.swing._;
import java.awt._;

abstract class Component
extends Object
with Reactor
{
  val acomponent: java.awt.Component

  def preferredSize = acomponent.getPreferredSize
  def preferredSize_=(x: Dimension) = acomponent.setPreferredSize(x)
  def preferredSize_=(xy: (Int, Int)) = acomponent.setPreferredSize(Dimension(xy._1, xy._2))

  def foreground: Color = new Color(acomponent.getForeground)
  def foreground_=(x: Color) = acomponent.setForeground(x)

  def background: Color = new Color(acomponent.getBackground)
  def background_=(x: Color) = acomponent.setBackground(x)

  def font: Font = acomponent.getFont
  def font_=(x: Font) = acomponent.setFont(x)

  def show: this.type = { acomponent.setVisible(true); this }
}
