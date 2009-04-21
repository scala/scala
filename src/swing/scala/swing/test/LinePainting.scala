package scala.swing.test

import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, geom}

import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleGUIApplication}
import scala.swing.event.{MousePressed, MouseDragged, MouseReleased}

/**
 * Dragging the mouse draws a simple graph
 *
 * @author Frank Teubler
 */
object LinePainting extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "SimpleDraw"
    contents = new Panel {
      background = Color.white
      preferredSize = new Dimension(200, 200)

      listenTo(Mouse.clicks, Mouse.moves)

      reactions += {
        case e: MousePressed  => moveTo(e.point)
        case e: MouseDragged  => lineTo(e.point)
        case e: MouseReleased => lineTo(e.point)
      }

      /* records the dragging */
      val path = new geom.GeneralPath

      def lineTo(p: Point) { path.lineTo(p.x, p.y); repaint() }
      def moveTo(p: Point) { path.moveTo(p.x, p.y); repaint() }

      override def paintComponent(g: Graphics) = {
        super.paintComponent(g)
        /* we need Graphics2D */
        val g2 = g.asInstanceOf[Graphics2D]
        g2.draw(path)
      }
    }
  }
}


