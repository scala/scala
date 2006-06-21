package scala.actors.gui;

import javax.swing._;
import java.awt._;

class Component(val acomponent: java.awt.Component) extends Subscriber {
  def show: this.type = { acomponent.setVisible(true); this }
}

