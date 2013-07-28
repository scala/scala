package scala.tools.nsc
package symtab

trait TopLevelDefinitions {
  self: SymbolTable =>

  import definitions._

  // Before ModuleDefs are eliminated, we can see the term names directly,
  // but afterward we have to rely on the symbol.
  case class TopLevelDefinition(pkg: String, tree: ImplDef) {
    def rawSymbol = tree.symbol match {
      case null | NoSymbol => NoSymbol
      case s               => s.initialize
    }
    def symbol                = rawSymbol
    def linkedSymbols         = List(symbol, symbol.sourceModule, symbol.moduleClass).distinct filter (_.exists)
    def companionSymbols      = List(symbol, symbol.companionSymbol) filter (_.exists)
    private def sym           = rawSymbol
    private val initialSymbol = rawSymbol
    private def flatName      = sym.name

    private def fixName(name: Name) = if (isModule) name.toTermName else name

    def hasSymbol         = sym != NoSymbol
    def hasOriginalSymbol = initialSymbol eq sym
    def owner             = if (sym eq NoSymbol) NoSymbol else sym.owner
    def size              = stats.size
    def mods              = tree.mods
    def stats             = tree.impl.body
    def isCaseCompanion   = isModule && mods.isSynthetic
    def isCase            = mods.isCase || sym.isCase
    def isImplClass       = mods.isTrait && sym.isImplClass
    def isTrait           = mods.isTrait && !isImplClass
    def prefix            = if (pkg == "") "" else s"$pkg."
    def fullName          = if (isPackageObject) pkg else s"$prefix$decoded"
    def name              = ( if (isPackageObject) newTermName((pkg split '.').last) else if (hasSymbol) sym.name else tree.name )
    def decoded           = name.decoded

    private def isPackageObject: Boolean = symbol.name.toTermName == nme.PACKAGEkw
    private def isModule = tree match {
      case ModuleDef(_, _, _) => true
      case _                  => sym.isModule || sym.isModuleClass
    }
    private def prekind = (
      if (isPackageObject) "package"
      else if (sym.isModuleClass) "<module>"
      else if (isImplClass) "<impl>"
      else if (isCaseCompanion) "<synth>"
      else if (isCase) "case"
      else ""
    )
    private def treeClass = TopLevelDefinition.shortClass(tree)
    private def symClass = if (sym eq NoSymbol) "" else TopLevelDefinition.shortClass(sym)
    private def classString = treeClass + ( if (symClass == "") "" else "/" + symClass )

    private def kind = (
      if (sym.isModule) "object"
      else if (mods.isTrait && !isImplClass) "trait"
      else "class"
    )
    private def fullKind = if (prekind == "") kind else s"$classString $prekind $kind"
    private def sizeString = if (size == 0) "" else f" ($size%-3d members)"

    def nameNoNumbers = fullName filterNot (_.isDigit)
    def nameNumber = (fullName.reverse.takeWhile(_.isDigit).reverse: String) match {
      case "" => 0
      case ds => ds.toInt
    }
    def symbolString        = if (hasOriginalSymbol) "" else s" (initial symbol was $initialSymbol, but now is $sym)"
    def implKind            = if (isModule) "object" else if (isImplClass) "iclass" else if (isTrait) "trait" else "class"
    def idString            = s"$fullKind $fullName"
    def longString          = f"$longStringNoMembers%50s  $sizeString"
    def longStringNoMembers = f"$numericId%-6s $classString%-27s $prekind%9s $kind%-6s $fullName%-50s"
    def numericId           = if (sym eq NoSymbol) "--" else sym.id.toString

    override def toString = longString
    override def equals(other: Any) = other match {
      case that: TopLevelDefinition => idString == that.idString
      case _                        => false
    }
    override def hashCode = idString.##
  }
  object TopLevelDefinition {
    private def changeString(before: Int, after: Int)(f: String => String): String = (
      if (before == after) ""
      else if (before < after) f("+" + (after - before))
      else f("" + (after - before))
    )

    private def relatedSym(label: String, s: Symbol): String = {
      if (s eq NoSymbol) "" else s"$label=$s#${s.id}"
    }
    private def symId(s: Symbol) = s"$s#${s.id}"
    private def shortClass(s: AnyRef): String = {
      def short(cl: Class[_]) = {
        val s    = if (cl eq null) "" else cl.getName
        val res  = (s split "[.$]").last
        val anon = res forall (_.isDigit)
        if (anon) "" else res
      }
      short(s.getClass) match {
        case "" => short(s.getClass.getInterfaces.last)
        case s  => s
      }
    }
    private def symbolString(s: Symbol) = {
      val c  = shortClass(s.initialize)
      val k  = s.accurateKindString
      val id = s.id
      val n  = s.fullName + "#" + id
      val r1 = relatedSym("moduleClass", s.moduleClass)
      val r2 = relatedSym("sourceModule", s.sourceModule)
      val r3 = relatedSym("companionSymbol", s.companionSymbol)
      val r = List(r1, r2, r3) filterNot (_ == "") mkString ", "
      val f  = s.debugFlagString match {
        case "" => s"a $c with no flags"
        case fs => s"a $fs $c"
      }

      f"$k%15s $n%-20s ($f) $r"
    }

    class DefinitionReporter {
      protected var trackMembers = false

      var prev: List[TopLevelDefinition] = Nil
      def prevSize(td: TopLevelDefinition) = prev find (_ == td) match {
        case Some(td) => td.size
        case _        => -1
      }
      def report(id: String, defns: List[TopLevelDefinition]) {
        val before = prev.toSet
        val after  = defns.toSet
        val all    = defns ++ (prev filterNot after) sorted

        def memberCountString(d: TopLevelDefinition): String = {
          val size1     = prevSize(d)
          val size2     = d.size
          val gain_s    = changeString(size1, size2)("  " + _)

          f"($size2%-3d members)" + gain_s
        }

        val descriptions = all map { d =>
          val marker = (
            if (before(d) && !after(d)) "-"
            else if (!before(d) && after(d)) "+"
            else if (trackMembers && prevSize(d) != d.size) "*"
            else " "
          )
          val pre = marker + "  " + d.longStringNoMembers
          pre + (marker match {
            case "*" => memberCountString(d)
            case _   => ""
          })
        }
        if (descriptions forall (_ startsWith " "))
          println(s"[$id] no changes")
        else {
          val prevString = changeString(before.size, after.size)("  (" + _ + ")")
          println(s"[$id] ${after.size} top level definitions$prevString")
          descriptions.sorted :+ "" foreach println
        }
        prev = defns
      }
    }

    implicit lazy val topLevelDefinitionOrdering: Ordering[TopLevelDefinition] =
      Ordering[(String, Int, String)] on (x => (x.nameNoNumbers, x.nameNumber, x.fullKind))

    private def exclude(name: Name) = name match {
      case tpnme.ANON_CLASS_NAME | tpnme.ANON_FUN_NAME => true
      case _                                           => (name endsWith nme.SPECIALIZED_SUFFIX)
    }
    private def search(tree: Tree, initialPackage: String): List[TopLevelDefinition] = {
      var pkgs: List[PackageDef] = (
        if (initialPackage == "") Nil
        else PackageDef(Ident(initialPackage), Nil) :: Nil
      )
      def currentPackage = pkgs reverseMap (_.pid.toString) mkString "."
      def inPackage[T](encl: PackageDef)(body: => T): T = {
        pkgs ::= encl
        try body finally pkgs = pkgs.tail
      }
      def mkDefn(impl: ImplDef): TopLevelDefinition = TopLevelDefinition(currentPackage, impl)
      def caseImpls(cd: ClassDef) = {
        if (cd.symbol != null && cd.symbol.isModuleClass)
          mkDefn(cd) :: Nil
        else {
          val mods = (cd.mods | Flags.SYNTHETIC) &~ Flags.CASE
          val mdef = ModuleDef(mods, cd.name.toTermName, Template(Nil, emptyValDef, Nil))

          mkDefn(cd) :: mkDefn(mdef) :: Nil
        }
      }
      def loop(t: Tree): List[TopLevelDefinition] = t match {
        case p @ PackageDef(_, stats)                     => inPackage(p)(stats flatMap loop)
        case impl: ImplDef if exclude(impl.name)          => Nil
        case cd @ ClassDef(mods, _, _, __) if mods.isCase => caseImpls(cd)
        case impl: ImplDef                                => mkDefn(impl) :: Nil
        case _                                            => Nil
      }
      loop(tree)
    }

    def inTree(tree: Tree): List[TopLevelDefinition]                    = inTree(tree, "")
    def inTree(tree: Tree, inPackage: String): List[TopLevelDefinition] = search(tree, inPackage)
  }
}
