import scala.reflect.{ClassTag, classTag}

/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

// This is an un-optimised example implementation


import scala.collection.mutable._

object meteor {
   def main(args: Array[String]) = {
      val solver = new Solver( Integer.parseInt(args(0)) )
      solver.findSolutions
      solver.printSolutions
   }
}




// Solver.scala
// import scala.collection.mutable._

final class Solver (n: Int) {
   private var countdown = n
   private var first: String = _
   private var last: String = _

   private val board = new Board()

   val pieces = Array(
      new Piece(0), new Piece(1), new Piece(2), new Piece(3), new Piece(4),
      new Piece(5), new Piece(6), new Piece(7), new Piece(8), new Piece(9) )

   val unplaced = new BitSet(pieces.length)

   { unplaced ++= (0 until pieces.length) }


   def findSolutions(): Unit = {
      if (countdown == 0) return

      if (unplaced.size > 0){
         val emptyCellIndex = board.firstEmptyCellIndex

         for (k <- Iterator.range(0,pieces.length)){
            if (unplaced.contains(k)){
               unplaced -= k

               for (i <- Iterator.range(0,Piece.orientations)){
                  val piece = pieces(k).nextOrientation

                  for (j <- Iterator.range(0,Piece.size)){
                     if (board.add(j,emptyCellIndex,piece)) {

                        if (!shouldPrune) findSolutions

                        board.remove(piece)
                     }
                  }
               }
               unplaced += k
            }
         }
      }
      else {
         puzzleSolved
      }
   }

   private def puzzleSolved() = {
      val b = board.asString
      if (first == null){
         first = b; last = b
      } else {
         if (b < first){ first = b } else { if (b > last){ last = b } }
      }
      countdown = countdown - 1
   }

   private def shouldPrune() = {
      board.unmark
      !board.cells.forall(c => c.contiguousEmptyCells % Piece.size == 0)
   }


   def printSolutions() = {

      def printBoard(s: String) = {
         var indent = false
         var i = 0
         while (i < s.length){
            if (indent) Console.print(' ')
            for (j <- Iterator.range(0,Board.cols)){
               Console.print(s.charAt(i)); Console.print(' ')
               i = i + 1
            }
            Console.print('\n')
            indent = !indent
         }
         Console.print('\n')
      }

      Console.print(n + " solutions found\n\n")
      printBoard(first)
      printBoard(last)
   }

/*
   def printPieces() =
      for (i <- Iterator.range(0,Board.pieces)) pieces(i).print
*/

}




// Board.scala
// import scala.collection.mutable._

object Board {
   val cols = 5
   val rows = 10
   val size = rows * cols
}

final class Board {
   val cells = boardCells()

   val cellsPieceWillFill = new Array[BoardCell](Piece.size)
   var cellCount = 0

   def unmark() = for (c <- cells) c.unmark

   def asString() =
      new String( cells map(
         c => if (c.piece == null) '-'.toByte
              else (c.piece.number + 48).toByte ))

   def firstEmptyCellIndex() = cells.findIndexOf(c => c.isEmpty)

   def add(pieceIndex: Int, boardIndex: Int, p: Piece) = {
      cellCount = 0
      p.unmark

      find( p.cells(pieceIndex), cells(boardIndex))

      val boardHasSpace = cellCount == Piece.size &&
         cellsPieceWillFill.forall(c => c.isEmpty)

      if (boardHasSpace) cellsPieceWillFill.foreach(c => c.piece = p)

      boardHasSpace
   }

   def remove(piece: Piece) = for (c <- cells; if c.piece == piece) c.empty


   private def find(p: PieceCell, b: BoardCell): Unit = {
      if (p != null && !p.marked && b != null){
         cellsPieceWillFill(cellCount) = b
         cellCount = cellCount + 1
         p.mark
         for (i <- Iterator.range(0,Cell.sides)) find(p.next(i), b.next(i))
      }
   }


   private def boardCells() = {
      val a = for (i <- Array.range(0,Board.size)) yield new BoardCell(i)
      val m = (Board.size / Board.cols) - 1

      for (i <- Iterator.range(0,a.length)){
         val row = i / Board.cols
         val isFirst = i % Board.cols == 0
         val isLast = (i+1) % Board.cols == 0
         val c = a(i)

         if (row % 2 == 1) {
            if (!isLast) c.next(Cell.NE) = a(i-(Board.cols-1))
            c.next(Cell.NW) = a(i-Board.cols)
            if (row != m) {
               if (!isLast) c.next(Cell.SE) = a(i+(Board.cols+1))
               c.next(Cell.SW) = a(i+Board.cols)
            }
         } else {
            if (row != 0) {
               if (!isFirst) c.next(Cell.NW) = a(i-(Board.cols+1))
               c.next(Cell.NE) = a(i-Board.cols)
            }
            if (row != m) {
               if (!isFirst) c.next(Cell.SW) = a(i+(Board.cols-1))
               c.next(Cell.SE) = a(i+Board.cols)
            }
         }
         if (!isFirst) c.next(Cell.W) = a(i-1)
         if (!isLast) c.next(Cell.E) = a(i+1)
      }
      a
   }


/*
// Printing all the board cells and their neighbours
// helps check that they are connected properly

   def printBoardCellsAndNeighbours() = {
      Console.println("cell\tNW NE W  E  SW SE")
      for (i <- Iterator.range(0,Board.size)){
         Console.print(i + "\t")
         for (j <- Iterator.range(0,Cell.sides)){
            val c = cells(i).next(j)
            if (c == null)
               Console.print("-- ")
            else
               Console.printf("{0,number,00} ")(c.number)
         }
         Console.println("")
      }
      Console.println("")
   }
*/

}




// Piece.scala

object Piece {
   val size = 5
   val rotations = Cell.sides
   val flips = 2
   val orientations = rotations * flips
}

final class Piece(_number: Int) {
   val number = _number
   val cells = for (i <- Array.range(0,Piece.size)) yield new PieceCell()

   {
      number match {
         case 0 => make0
         case 1 => make1
         case 2 => make2
         case 3 => make3
         case 4 => make4
         case 5 => make5
         case 6 => make6
         case 7 => make7
         case 8 => make8
         case 9 => make9
      }
   }

   def flip() = for (c <- cells) c.flip
   def rotate() = for (c <- cells) c.rotate
   def unmark() = for (c <- cells) c.unmark


   private var orientation = 0

   def nextOrientation() = {
      if (orientation == Piece.orientations) orientation = 0
      if (orientation % Piece.rotations == 0) flip else rotate
      orientation = orientation + 1
      this
   }


   private def make0() = {
      cells(0).next(Cell.E) = cells(1)
      cells(1).next(Cell.W) = cells(0)
      cells(1).next(Cell.E) = cells(2)
      cells(2).next(Cell.W) = cells(1)
      cells(2).next(Cell.E) = cells(3)
      cells(3).next(Cell.W) = cells(2)
      cells(3).next(Cell.SE) = cells(4)
      cells(4).next(Cell.NW) = cells(3)
   }

   private def make1() = {
      cells(0).next(Cell.SE) = cells(1)
      cells(1).next(Cell.NW) = cells(0)
      cells(1).next(Cell.SW) = cells(2)
      cells(2).next(Cell.NE) = cells(1)
      cells(2).next(Cell.W) = cells(3)
      cells(3).next(Cell.E) = cells(2)
      cells(3).next(Cell.SW) = cells(4)
      cells(4).next(Cell.NE) = cells(3)
   }

   private def make2() = {
      cells(0).next(Cell.W) = cells(1)
      cells(1).next(Cell.E) = cells(0)
      cells(1).next(Cell.SW) = cells(2)
      cells(2).next(Cell.NE) = cells(1)
      cells(2).next(Cell.SE) = cells(3)
      cells(3).next(Cell.NW) = cells(2)
      cells(3).next(Cell.SE) = cells(4)
      cells(4).next(Cell.NW) = cells(3)
   }

   private def make3() = {
      cells(0).next(Cell.SW) = cells(1)
      cells(1).next(Cell.NE) = cells(0)
      cells(1).next(Cell.W) = cells(2)
      cells(2).next(Cell.E) = cells(1)
      cells(1).next(Cell.SW) = cells(3)
      cells(3).next(Cell.NE) = cells(1)
      cells(2).next(Cell.SE) = cells(3)
      cells(3).next(Cell.NW) = cells(2)
      cells(3).next(Cell.SE) = cells(4)
      cells(4).next(Cell.NW) = cells(3)
   }

   private def make4() = {
      cells(0).next(Cell.SE) = cells(1)
      cells(1).next(Cell.NW) = cells(0)
      cells(1).next(Cell.SW) = cells(2)
      cells(2).next(Cell.NE) = cells(1)
      cells(1).next(Cell.E) = cells(3)
      cells(3).next(Cell.W) = cells(1)
      cells(3).next(Cell.SE) = cells(4)
      cells(4).next(Cell.NW) = cells(3)
   }

   private def make5() = {
      cells(0).next(Cell.SW) = cells(1)
      cells(1).next(Cell.NE) = cells(0)
      cells(0).next(Cell.SE) = cells(2)
      cells(2).next(Cell.NW) = cells(0)
      cells(1).next(Cell.SE) = cells(3)
      cells(3).next(Cell.NW) = cells(1)
      cells(2).next(Cell.SW) = cells(3)
      cells(3).next(Cell.NE) = cells(2)
      cells(3).next(Cell.SW) = cells(4)
      cells(4).next(Cell.NE) = cells(3)
   }

   private def make6() = {
      cells(0).next(Cell.SW) = cells(1)
      cells(1).next(Cell.NE) = cells(0)
      cells(2).next(Cell.SE) = cells(1)
      cells(1).next(Cell.NW) = cells(2)
      cells(1).next(Cell.SE) = cells(3)
      cells(3).next(Cell.NW) = cells(1)
      cells(3).next(Cell.SW) = cells(4)
      cells(4).next(Cell.NE) = cells(3)
   }

   private def make7() = {
      cells(0).next(Cell.SE) = cells(1)
      cells(1).next(Cell.NW) = cells(0)
      cells(0).next(Cell.SW) = cells(2)
      cells(2).next(Cell.NE) = cells(0)
      cells(2).next(Cell.SW) = cells(3)
      cells(3).next(Cell.NE) = cells(2)
      cells(3).next(Cell.SE) = cells(4)
      cells(4).next(Cell.NW) = cells(3)
   }

   private def make8() = {
      cells(0).next(Cell.E) = cells(1)
      cells(1).next(Cell.W) = cells(0)
      cells(1).next(Cell.E) = cells(2)
      cells(2).next(Cell.W) = cells(1)
      cells(2).next(Cell.NE) = cells(3)
      cells(3).next(Cell.SW) = cells(2)
      cells(3).next(Cell.E) = cells(4)
      cells(4).next(Cell.W) = cells(3)
   }

   private def make9() = {
      cells(0).next(Cell.E) = cells(1)
      cells(1).next(Cell.W) = cells(0)
      cells(1).next(Cell.E) = cells(2)
      cells(2).next(Cell.W) = cells(1)
      cells(2).next(Cell.NE) = cells(3)
      cells(3).next(Cell.SW) = cells(2)
      cells(2).next(Cell.E) = cells(4)
      cells(4).next(Cell.W) = cells(2)
      cells(4).next(Cell.NW) = cells(3)
      cells(3).next(Cell.SE) = cells(4)
   }

/*
   def print() = {
      Console.println("Piece # " + number)
      Console.println("cell\tNW NE W  E  SW SE")
      for (i <- Iterator.range(0,Piece.size)){
         Console.print(i + "\t")
         for (j <- Iterator.range(0,Cell.sides)){
            val c = cells(i).next(j)
            if (c == null)
               Console.print("-- ")
            else
               for (k <- Iterator.range(0,Piece.size)){
                  if (cells(k) == c) Console.printf(" {0,number,0} ")(k)
               }
         }
         Console.println("")
      }
      Console.println("")
   }
*/

}




// Cell.scala

object Cell {
   val NW = 0; val NE = 1
   val W  = 2; val E  = 3
   val SW = 4; val SE = 5

   val sides = 6
}

abstract class Cell {
  implicit def t: ClassTag[T]
  type T
  val next = new Array[T](Cell.sides)
  var marked = false

  def mark() = marked = true
  def unmark() = marked = false
}

// BoardCell.scala

final class BoardCell(_number: Int) extends {
   type T = BoardCell
   implicit val t = classTag[BoardCell]
} with Cell {
   val number = _number
   var piece: Piece = _

   def isEmpty() = piece == null
   def empty() = piece = null

   def contiguousEmptyCells(): Int = {
      if (!marked && isEmpty){
         mark
         var count = 1

         for (neighbour <- next)
            if (neighbour != null && neighbour.isEmpty)
               count = count + neighbour.contiguousEmptyCells

         count } else { 0 }
   }
}




// PieceCell.scala

final class PieceCell extends Cell {
   type T = PieceCell

   def flip = {
      var swap = next(Cell.NE)
      next(Cell.NE) = next(Cell.NW)
      next(Cell.NW) = swap

      swap = next(Cell.E)
      next(Cell.E) = next(Cell.W)
      next(Cell.W) = swap

      swap = next(Cell.SE)
      next(Cell.SE) = next(Cell.SW)
      next(Cell.SW) = swap
   }

   def rotate = {
      var swap = next(Cell.E)
      next(Cell.E) = next(Cell.NE)
      next(Cell.NE) = next(Cell.NW)
      next(Cell.NW) = next(Cell.W)
      next(Cell.W) = next(Cell.SW)
      next(Cell.SW) = next(Cell.SE)
      next(Cell.SE) = swap
   }
}



