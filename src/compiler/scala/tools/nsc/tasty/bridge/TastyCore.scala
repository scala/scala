package scala.tools.nsc.tasty.bridge

import scala.tools.nsc
import nsc.symtab, nsc.tasty.TastyUniverse

abstract class TastyCore { self: TastyUniverse =>
  import self.{symbolTable => u}

  // Compiler Entry Point
  type SymbolTable <: symtab.SymbolTable { def settings: nsc.Settings }
  val symbolTable: SymbolTable

  // Misc
  type FlagSet = u.FlagSet
  type Position = u.Position
  type Scope = u.Scope
  type Constant = u.Constant
  type Mirror = u.Mirror
  type Annotation = u.Annotation
  type Phase = reflect.internal.Phase
  type TypeError = u.TypeError

  // Types
  type Type               = u.Type
  type ClassInfoType      = u.ClassInfoType
  type ExistentialType    = u.ExistentialType
  type NullaryMethodType  = u.NullaryMethodType
  type MethodType         = u.MethodType
  type PolyType           = u.PolyType
  type ThisType           = u.ThisType
  type TypeRef            = u.TypeRef
  type SingleType         = u.SingleType
  type AnnotatedType      = u.AnnotatedType
  type TypeBounds         = u.TypeBounds
  type RefinedType        = u.RefinedType
  type ConstantType       = u.ConstantType

  // Symbols
  type Symbol                = u.Symbol
  type MethodSymbol          = u.MethodSymbol
  type TermSymbol            = u.TermSymbol
  type ModuleSymbol          = u.ModuleSymbol
  type ClassSymbol           = u.ClassSymbol
  type FreeTypeSymbol        = u.FreeTypeSymbol
  type RefinementClassSymbol = u.RefinementClassSymbol

  // Names
  type Name     = u.Name
  type TermName = u.TermName
  type TypeName = u.TypeName

  // Trees
  type SingletonTypeTree  = u.SingletonTypeTree
  type Apply              = u.Apply
  type TypeApply          = u.TypeApply
  type AppliedTypeTree    = u.AppliedTypeTree
  type Super              = u.Super
  type TypeBoundsTree     = u.TypeBoundsTree
  type CompoundTypeTree   = u.CompoundTypeTree
  type NamedArg           = u.NamedArg
  type Block              = u.Block
  type Annotated          = u.Annotated
  type Tree               = u.Tree
  type RefTree            = u.RefTree
  type TypeTree           = u.TypeTree
  type This               = u.This
  type SeqLiteral         = u.ArrayValue
  type Typed              = u.Typed
  type Literal            = u.Literal
  type Ident              = u.Ident
  type New                = u.New
  type If                 = u.If
  type Select             = u.Select

}
