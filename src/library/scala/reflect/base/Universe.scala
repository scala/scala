package scala.reflect
package base

abstract class Universe extends Symbols
                           with Types
                           with FlagSets
                           with Scopes
                           with Names
                           with Trees
                           with Constants
                           with AnnotationInfos
                           with Positions
                           with TypeTags
                           with TagInterop
                           with StandardDefinitions
                           with StandardNames
                           with BuildUtils
                           with Mirrors