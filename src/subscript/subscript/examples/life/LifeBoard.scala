package life

import java.awt.{Graphics, Image, Color, Point}
import java.awt.image.{BufferedImage}
import javax.swing.{JPanel}
import scala.swing._
import scala.swing.event._

class LifeBoard(var cellColumns: Int = 200, var cellRows: Int = 150) extends Panel {
    type CellState = Boolean
    val cellSizeX = 3
    val cellSizeY = 3
    var currentX  = -1
    var currentY  = -1
    
    var myForegroundColor = java.awt.Color.black
    var myBackgroundColor = java.awt.Color.white

    var cells        : Array[Array[CellState]] = null
    var bufferedImage: BufferedImage    = null
    var imageGC      : Graphics = null
    
    doClear

    //////////////////////////////////////////////
    // functions for mouse input
    //////////////////////////////////////////////

    def resetLastMousePos         = currentX = -1
    def mouseDownToggle(p: Point) = toggleAtMousePoint(p.x,p.y)
    def mouseDragSet   (p: Point) = lineToMousePoint(p.x,p.y)
                                  

    def toggleAtMousePoint (x: Int, y: Int) {
        if (bufferedImage==null) createCells
        if (bufferedImage==null) return
        invertCell (x/cellSizeX, y/cellSizeY)
    }
    def setAtMousePoint (x: Int, y: Int) {
        if (bufferedImage==null) createCells
        if (bufferedImage==null) return
        setCell (x/cellSizeX, y/cellSizeY)
    }
    
    def lineToMousePoint (mx: Int, my: Int) {
      val cx = mx/cellSizeX
      val cy = my/cellSizeY
      if (currentX == -1) setAtMousePoint(mx, my)
      else {
        val deltaX = cx - currentX
        val deltaY = cy - currentY
        linedda(currentX, currentY, cx, cy)
      }
      currentX = cx
      currentY = cy
    }
    
    def linedda(xa: Int, ya: Int, xb: Int, yb: Int) { // http://wiki.answers.com/Q/DDA_line_algorithm
      val  dx = xb - xa
      val  dy = yb - ya
      val adx = Math.abs(dx)
      val ady = Math.abs(dy)
      val steps = if(adx>ady) adx else ady
      
      val dsteps: Float = steps
      var      x: Float = xa
      var      y: Float = ya
      val xincrement     = dx / dsteps
      val yincrement     = dy / dsteps
      setCell (Math.round(x),Math.round(y)) 
      for(k <- 0 to steps) {
        x += xincrement 
        y += yincrement 
        setCell (Math.round(x),Math.round(y)) 
      } 
    }

    //////////////////////////////////////////////
    // functions for painting and the like
    //////////////////////////////////////////////
    def validate {

        //super.validate
        val newWidth  = size.width
        val newHeight = size.height;
        if (bufferedImage == null) {
            createCells
        } else if (   bufferedImage.getWidth(null)  != newWidth 
                   || bufferedImage.getHeight(null) != newHeight) {
             createCells
        } else {
          renderBufferedImage
        }
        repaint
    }

    def cleanUp {
        if (bufferedImage == null) return
        imageGC.dispose
        bufferedImage.flush
        bufferedImage = null
    }

    /** randomize the canvas with the given density */
    def doRandomize(density: Double = 0.3): Unit = {
        if (bufferedImage==null) createCells
        val random = new java.security.SecureRandom
        cells = Array.fill(cellColumns, cellRows) { random.nextInt(1000) < 1000*density }                       
        createBufferedImage
        renderBufferedImage
        repaint
    }
    def doClear          = doRandomize(0.0)
    def getPreferredSize = new Dimension (cellSizeX*cellColumns, cellSizeY*cellRows);

    def renderBufferedImage {
        if (bufferedImage == null) return
        for (cellX <- 0 until cellColumns;
             cellY <- 0 until cellRows) {
             paintCellInImage (cellX, cellY)
        }
    }
    def update(g: Graphics2D) {
      paint (g);
    }
    
    override def paint (g: Graphics2D) {
        if (bufferedImage == null) {createCells; renderBufferedImage}
        g.drawImage (bufferedImage, 0, 0, null)
    }

    def paintCellInImage(cellX: Int, cellY: Int) {
       imageGC.setColor(if (cells(cellX)(cellY)) myForegroundColor else myBackgroundColor)
       imageGC. fillRect(cellX*cellSizeX, cellY*cellSizeY, cellSizeX, cellSizeY)
    }

    //////////////////////////////////////////////
    // functions implementing the life algorithm
    //////////////////////////////////////////////

    def createCells: Unit = createCells0(size.width, size.height)
    def createCells0 (width: Int, height: Int): Unit = {
//        if (width==0
//        || height==0)
//        {
//            return 
//        }
//        cellColumns = width  / cellSizeX
//        cellRows    = height / cellSizeY
        cells         = Array.ofDim[CellState](cellColumns, cellRows)
        createBufferedImage
    }
        
    def createBufferedImage {
        cleanUp
        if (size.width <= 0) return
        bufferedImage = new BufferedImage(size.width, size.height, BufferedImage.TYPE_INT_RGB);
        imageGC       = bufferedImage.getGraphics
        imageGC.setColor(myBackgroundColor)
        imageGC.fillRect (0,0,bufferedImage.getWidth (null)-1,
                               bufferedImage.getHeight(null)-1)
    }

    def invertCell (cellX: Int, cellY: Int) {
        if (     cellX < 0 || cellX >= cellColumns 
              || cellY < 0 || cellY >= cellRows)
            return;

        setCellValue(cellX, cellY, !cells(cellX)(cellY))
    }
    def setCell (cellX: Int, cellY: Int) {
        if (     cellX < 0 || cellX >= cellColumns 
              || cellY < 0 || cellY >= cellRows)
            return;

        if (cells(cellX)(cellY)) return

        setCellValue(cellX, cellY, true)
    }

    def setCellValue(cellX: Int, cellY: Int, value: Boolean) {
        if (     cellX < 0 || cellX >= cellColumns 
              || cellY < 0 || cellY >= cellRows)
            return;
        cells(cellX)(cellY) = value
        
        if (bufferedImage == null) return

        paintCellInImage (cellX, cellY)
        repaint(new Rectangle(cellX*cellSizeX, cellY*cellSizeY, cellSizeX, cellSizeY))
    }

    def calculateGeneration {
        val neighbors = Array.ofDim[Int](2+cellColumns, 2+cellRows)

        for (cellX <- 0 until cellColumns; 
             cellY <- 0 until cellRows   ) {
                if (cells(cellX)(cellY)) {
                    neighbors(1+cellX-1)(1+cellY-1) += 1
                    neighbors(1+cellX  )(1+cellY-1) += 1
                    neighbors(1+cellX+1)(1+cellY-1) += 1
                    neighbors(1+cellX-1)(1+cellY )  += 1
                    neighbors(1+cellX+1)(1+cellY )  += 1
                    neighbors(1+cellX-1)(1+cellY+1) += 1
                    neighbors(1+cellX  )(1+cellY+1) += 1
                    neighbors(1+cellX+1)(1+cellY+1) += 1
                }
        }
        for (cellX <- 0 until cellColumns;
             cellY <- 0 until cellRows) {
             val n = neighbors(1+cellX)(1+cellY)
             if (n!=2) cells(cellX)(cellY) = n == 3
        }
    }
}
