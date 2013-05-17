package examples.swing

import java.awt.{Color, Font, Dimension}
import swing._
import event._
import Swing._
import BorderPanel._

/**
 * Demo for ColorChooser.
 * Based on http://download.oracle.com/javase/tutorial/uiswing/components/colorchooser.html
 * 
 * @author andy@hicks.net
 */
object ColorChooserDemo extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "ColorChooser Demo"
    size = new Dimension(400, 400)
    
    contents = ui
  }

  def ui = new BorderPanel {
    val colorChooser = new ColorChooser {
      reactions += {
        case ColorChanged(_, c) =>
          banner.foreground = c
      }
    }

    colorChooser.border = TitledBorder(EtchedBorder, "Choose Text Color")
    
    val banner = new Label("Welcome to Scala Swing") {
      horizontalAlignment = Alignment.Center
      foreground = Color.yellow
      background = Color.blue
      opaque = true
      font = new Font("SansSerif", Font.BOLD, 24)
    }
   
    val bannerArea = new BorderPanel {
      layout(banner) = Position.Center
      border = TitledBorder(EtchedBorder, "Banner")
    }
    
    // Display a color selection dialog when button pressed 
    val selectColor = new Button("Choose Background Color") {
      reactions += {
        case ButtonClicked(_) =>
          ColorChooser.showDialog(this, "Test", Color.red) match {
            case Some(c) => banner.background = c
            case None =>
          }
      }
    }

    layout(bannerArea) = Position.North
    layout(colorChooser) = Position.Center
    layout(selectColor) = Position.South
  }
}