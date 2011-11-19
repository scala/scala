package scala.reflect
package api

abstract class Universe extends Symbols
                           with Types
                           with Constants
                           with Scopes
                           with Names
                           with Trees
                           with Positions
                           with TreePrinters
                           with AnnotationInfos
                           with StandardDefinitions {
  type Position
  val NoPosition: Position

}

