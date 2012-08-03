package scala.reflect
package api

abstract class Universe extends base.Universe
                           with Symbols
                           with Types
                           with FlagSets
                           with Names
                           with Trees
                           with Printers
                           with Constants
                           with Positions
                           with Mirrors
                           with StandardDefinitions
                           with StandardNames
                           with Importers
                           with AnnotationInfos
