/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.symtab.Symbol;
import scalac.symtab._;
import scalac.symtab.Definitions;
import scalac.Global;
import scalac.util.Names;

package scala.tools.scaladoc {

  import scala.collection.immutable._;

  /**
  * @author Vincent Cremet
  * @author Roland Tchakoute
  */
  abstract class ML {

    ////////////// TYPE CONSTRUCTORS ////////////////

    trait TypeConstructor[T <: TypeConstructor[T]] {
      def sameAs(that: T): boolean;
    }
    type TC <: TypeConstructor[TC];

    trait MLType {
      override def toString(): String = match {
        case TypeVar(n) => "x" + n
        case UnitType => "unit"
        case AtomicType(c, args) => c.toString() + "[" + args.mkString("", ", ","") + "]"
        case ProdType(l, r) => "(" + l + " * " + r + ")"
        case FunType(arg, res) => "(" + arg.toString() + " -> " + res + ")"
      }
      def * (that: MLType) = ProdType(this, that);
      def -> (that: MLType) = FunType(this, that);
    }
    def x(n: int) = TypeVar(n);
    val unt = UnitType;
    def at(c: TC, args: MLType*) = AtomicType(c, args.asInstanceOf[List[MLType]]);

    case class TypeVar(n: int) extends MLType;
    case object UnitType extends MLType;
    case class AtomicType(c: TC, args: List[MLType]) extends MLType;
    case class ProdType(l: MLType, r: MLType) extends MLType;
    case class FunType(arg: MLType, res: MLType) extends MLType;

    val fullIso = true;

    ///////////////// NORMAL FORMS ////////////////////

    /** Transform  a type into his normal form. */
    def normalForm(t: MLType): MLType = {
      def normalFormAux(t: MLType): MLType = t match {
        case FunType(arg, UnitType) => if (fullIso) UnitType else t
        case FunType(UnitType, res) => res
        case ProdType(l, UnitType) => l
        case ProdType(UnitType, r) => r
        case FunType(ProdType(l, r), res) =>
          normalFormAux(FunType(l, normalFormAux(FunType(r, res))))
        case FunType(arg, ProdType(l,r)) =>
          ProdType(normalFormAux(FunType(arg, l)), normalFormAux(FunType(arg, r)))
        case _ => t
      }
      t match {
        case ProdType(l, r) =>
          normalFormAux(ProdType(normalForm(l), normalForm(r)))
        case FunType (arg, res) =>
          normalFormAux(FunType(normalForm(arg), normalForm(res)))
        case _ => t
      }
    }

    type Type_coords = Pair[int, List[MLType]];

    def rewriteType(t: MLType): Type_coords = {
      def flatten(t: MLType): Type_coords = t match {
        case ProdType(l,r) => {
          val Pair(len1, l1) = flatten(l);
          val Pair(len2, l2) = flatten(r);
          Pair(len1 + len2, l1 ::: l2)
        }
        case _ => Pair(1, List(t))
      }
      flatten(normalForm(t))
    }

    /////////////////// RENAMINGS ////////////////////////

    type Renaming = Map[int,int];

    def rename_type(ren: Renaming): MLType => MLType = {
      if (ren.isEmpty)
        (x => x)
      else {
        case TypeVar(n) => TypeVar(ren(n))
        case UnitType => UnitType
        case AtomicType(c, args) => AtomicType(c, args map rename_type(ren))
        case ProdType(l, r) => ProdType(rename_type(ren)(l), rename_type(ren)(r))
        case FunType(arg, res) => FunType(rename_type(ren)(arg), rename_type(ren)(res))
      }
    }

    ///////////////// SPLIT NORMAL FORMS ///////////////////


    def fold[a,b](l: List[a], z: b, op: (b, a) => Pair[b,a]): Pair[b, List[a]] =
      l match {
        case List() => Pair(z, List())
        case x :: xs => {
          val Pair(z1, x1) = op(z, x);
          val Pair(z2, xs1) = fold(xs, z1, op);
          Pair(z2, x1 :: xs1)
        }
      }

    def containsProdType(t: MLType): boolean = t match {
      case ProdType(_, _) => true
      case TypeVar(_) => false
      case UnitType => false
      case AtomicType(_, _) => false
      case FunType(arg, res) => containsProdType(arg) || containsProdType(res)
    }

    def split_vars(start: int, t: Type_coords): Pair[Pair[int, Renaming], Type_coords] = {
      /**
      * Take a coordinate and return a copy with type variables renamed and
      * keeping track of the renaming.
      */
      def shift_compact_vars(p: Pair[int, Renaming], t: MLType) : Pair[Pair[int, Renaming], MLType] = {
        if (containsProdType(t))
          Console.println("SPLIT_VARS: " + t);
        val Pair(start, ren) = p;
        t match {
          case TypeVar(n) =>
            if (ren.contains(n))
              Pair(Pair(start, ren), TypeVar(ren(n)))
	    else
              Pair(Pair(start + 1, ren.incl(Pair(n, start))), TypeVar(start))
          case UnitType => Pair(p, t)
          case FunType(arg, res) => {
	    val Pair(Pair(new_start, new_ren), new_arg) = shift_compact_vars(Pair(start, ren), arg);
	    val Pair(q, new_res) = shift_compact_vars(Pair(new_start, new_ren), res);
	    Pair(q, FunType(new_arg, new_res))
          }
          case AtomicType(c, args) => {
	    val Pair(p, new_args) = fold(args, Pair(start, ren), shift_compact_vars);
	    Pair(p, AtomicType(c, new_args))
          }
        }
      }
      def shifter(p: Pair[int, Renaming], coord: MLType): Pair[Pair[int, Renaming], MLType] = {
        val Pair(start, ren) = p;
        val Pair(Pair(new_start, local_ren), new_coord) =
          shift_compact_vars(Pair(start, ListMap.Empty), coord);
        Pair(Pair(new_start, ren incl local_ren), new_coord)
      }

      val Pair(len, coords) = t;
      val Pair(p, ren_coords) = fold(coords, Pair(start, ListMap.Empty[int, int]), shifter);
      Pair(p, Pair(len , ren_coords))
    }

    def splitRewrite(start: int, t: MLType): Pair[Pair[int, Renaming], Type_coords] =
      split_vars(start, rewriteType(t));

    /////////////// FUNCTIONAL TYPE FLATENING ////////////////

    type Head_type = Pair[int, MLType];
    // type Flat_type = Pair[Head_type, List[Flat_type]];
    trait Flat_type ;
    case class Fl(res: Head_type, args: List[Flat_type]) extends Flat_type;

    def flatten (t: MLType): Flat_type = t match {
      case FunType(args1, FunType(args2, res)) => {
        val f1 = flatten(args1);
        val f2 = flatten(args2);
        val Fl(Pair(len, head), tail) = flatten(res);
        Fl(Pair(len + 2, head), f1 :: f2 :: tail)
      }
      case FunType(args,res) => Fl(Pair(1, res), List(flatten(args)))
      case _ => Fl(Pair(0, t), List());
    }

    ////////////// PERMUTATIONS ///////////////////

    def perms[a](l: List[a]): List[List[a]] = {
      def insert(y: a)(l: List[a]): List[List[a]] =
        l match {
          case List() => List(List(y))
          case x :: xs => (y :: l) :: (insert(y)(xs) map { ys => x :: ys })
        }
      l match {
        case List() => List(List())
        case x :: xs => perms(xs) flatMap insert(x)
      }
    }

    ///////////////// FUNCTIONAL TYPE UNIFICATION ////////////////

    /** Global renaming. */
    var  globalRen: Renaming = ListMap.Empty;

    def rename_var(x1: int, x2: int): boolean =
      Pair(globalRen.get(x1), globalRen.get(x2)) match {
        case Pair(Some(y1), Some(y2)) => y1 == x2
        case Pair(None, None) => {
          globalRen = globalRen.incl(Pair(x1, x2), Pair(x2, x1));
          true
        }
        case _ => false
      }

    def unify_LC (f1: Flat_type, f2: Flat_type): boolean = {
      val Fl(Pair(len1, res1), args1) = f1;
      val Fl(Pair(len2, res2), args2) = f2;
      if(len1 != len2)
        false
      else {
        val saved_renaming = globalRen;
        if (unify_head(res1, res2))
	  unify_list(args1, args2)
        else {
	  globalRen = saved_renaming;
	  false
        }
      }
    }

    case object UnifyHeadExc extends Exception;

    def unify_head(t1: MLType,t2: MLType): boolean = {
      def unify_rec(p: Pair[MLType, MLType]): unit = p match {
        case Pair(UnitType, UnitType) => ()
        case Pair(TypeVar(n), TypeVar(m)) =>
          if (rename_var(n,m))
            ()
          else
            throw  UnifyHeadExc
        case Pair(_, TypeVar(_)) => throw UnifyHeadExc;
        case Pair(TypeVar(_), _) => throw UnifyHeadExc;
        case Pair(AtomicType(c1, args1), AtomicType(c2, args2)) =>
          if (c1 sameAs c2)
            (args1 zip args2) foreach unify_rec
          else
            throw UnifyHeadExc
        case _ => throw UnifyHeadExc
      }
      try{
        unify_rec(Pair(t1, t2));
        true
      }
      catch {
        case UnifyHeadExc => false
      }
    }

    case object FoundExc extends Exception;

    def unify_list(fs1: List[Flat_type], fs2: List[Flat_type]): boolean = {
      try{
        perms(fs1) foreach  {
          fs => {
            val saved_renaming = globalRen;
	    unify_map(fs, fs2);
	    globalRen = saved_renaming;
          }
        }
        false
      }
      catch {
        case FoundExc => true;
      }
    }

    def unify_map (fs1: List[Flat_type], fs2: List[Flat_type]): boolean =
      Pair(fs1, fs2) match {
        case Pair(List(), List()) => throw FoundExc
        case Pair(a :: resta, b :: restb) =>
          if (unify_LC(a, b))
	    unify_map (resta, restb)
          else
            false
      }

    def unify_left_commutative(f1: Flat_type, f2: Flat_type): Pair[boolean, Renaming] = {
      globalRen = ListMap.Empty;
      val unifiable = unify_LC(f1, f2);
      Pair(unifiable, globalRen)
    }

    def findiso(fs: List[Flat_type], f: Flat_type): Triple[Boolean, List[Flat_type], Renaming] =
      fs match {
        case List() => Triple(false, List(), ListMap.Empty)
        case  g :: gs => {
          val Pair(unifiable, ren) = unify_left_commutative(f, g);
          if (unifiable)
            Triple(true, gs, ren)
          else {
	    val Triple(b, rest, ren) = findiso(gs, f);
	    Triple(b, g :: rest, ren)
          }
        }
      }

    def quadratic_test(a: List[Flat_type], b: List[Flat_type]): Option[Renaming] = {
      var renaming: Renaming = ListMap.Empty;
      def q_test(a: List[Flat_type], b: List[Flat_type]): Option[Renaming] =
        Pair(a,b) match {
          case Pair(List(), List()) => Some(renaming)
          case Pair(List(), _) => None
          case Pair(_, List()) => None
          case Pair(fs, g :: gs) => {
            val Triple(b, rest, ren) = findiso(fs, g);
            if (b) {
	      renaming = ren incl renaming;
	      q_test(rest, gs)
	    }
            else
              None
          }
        }
      q_test(a, b)
    }

    def iso(a: MLType): MLType => Triple[Option[Renaming], Renaming, Renaming] = {
      val Pair(Pair(start_a, ren_a), Pair(len_a, coords_a)) = splitRewrite(1, a);
      val flat_coords_a = coords_a map flatten;
      {
        b => {
          val Pair(Pair(_, ren_b), Pair(len_b, coords_b)) = splitRewrite(start_a, b);
          if (len_a != len_b)
            Triple(None, ren_a, ren_b)
          else{
            val flat_coords_b = coords_b map flatten;
            Triple(quadratic_test(flat_coords_a, flat_coords_b), ren_a, ren_b)
          }
        }
      }
    }

    def build_renaming(ren_unif: Renaming, ren_a: Renaming, ren_b: Renaming): Renaming = {
      def ren_builder(r: Renaming, ren_a: Renaming)(key: int, value: int) = ren_a(r(value));
      def inverse(ren: Renaming): Renaming = (ren foldLeft (ListMap.Empty: Renaming)) {
        (ren_inv, p) => (ren_inv + p._2 -> p._1)
      }
      ren_b map (ren_builder(ren_unif, inverse(ren_a)))
    }

    def printIso(t: MLType,u: MLType): unit  = {
      Console.println("TEST ISO");
      Console.println(t.toString());
      Console.println(u.toString());
      iso(t)(u) match {
        case Triple(None, _, _) =>
          Console.println("Not isomorphic.");
        case Triple(Some(ren_unif), ren_a, ren_b) => {
          Console.println("Isomorphic:");
          val response = rename_type(build_renaming(ren_unif, ren_a, ren_b))(u);
          Console.println(response.toString());
        }
      }
    }

  }

  class ScalaML(global: Global) extends ML with TypeIsomorphism {

    class ScalaTC(sym: Symbol) extends TypeConstructor[ScalaTC] {
      val symbol: Symbol = sym;
      def sameAs(that: ScalaTC): boolean = (symbol == that.symbol);
      override def toString(): String = symbol.nameString();
    }

    type TC = ScalaTC;

    /** Build a product type from a list of types. */
    def mkProd(l: List[MLType]): MLType = l match {
      case List() => UnitType
      case t :: List() => t
      case t :: ts => ProdType(t, mkProd(ts))
    }

    ////////////////////// TESTS /////////////////////

    /** Test if this type is "scala.Unit". */
    def is_scala_Unit(t: Type): boolean =
      t.symbol() == Global.instance.definitions.UNIT_CLASS;

    /** Test if this type is "scala.Tuple_n[T_1, ..., T_n]". */
    def is_scala_Tuple(t: Type): boolean = {
      val tuples = Global.instance.definitions.TUPLE_CLASS;
      val arity = t.typeArgs().length;
      arity < tuples.length && t.symbol() == tuples(arity);
    }

    /** Test if this type is "scala.Function_n[T_1, ..., T_n, T]". */
    def is_scala_Function(t: Type): boolean = {
      val functions = Global.instance.definitions.FUNCTION_CLASS;
      val arity = t.typeArgs().length - 1;
      0 <= arity && arity < functions.length && t.symbol() == functions(arity);
    }

    /** Test if this type is a local variable */
    def is_localVariable(t: Type): boolean =
      t.symbol().isAbstractType() && t.symbol().isParameter();

    case object TranslateExc extends Exception;

    /** Try to translate a Scala type into an ML type. */
    def translate(t: Type): Option[Pair[MLType, Map[Symbol, int]]] = {

      // global renaming
      var renaming: Map[Symbol, int] = ListMap.Empty;
      def renamed(sym: Symbol) =
        if (renaming.contains(sym))
          renaming(sym)
        else {
          val len = renaming.size;
          renaming = renaming.incl(Pair(sym, len));
          len
        }

      def listOfArray[a](r: Array[a]): List[a] =
        List.fromArray(r, 0, r.length);

      def trans(t: Type): MLType =
        // scala.Unit
        if (is_scala_Unit(t))
          UnitType
      // scala.Tuple_n[T_1, ..., T_n]
        else if (is_scala_Tuple(t))
          t match {
            case Type$TypeRef(_, _, args) => mkProd(listOfArray(args) map trans)
          }
      // scala.Function_n[T_1, ..., T_n, T]
        else if (is_scala_Function(t))
          t match {
            case Type$TypeRef(_, _, args) => {
              val params = listOfArray(args);
              FunType(mkProd(params.init map trans), trans(params.last))
            }
          }
      // local variable
        else if (is_localVariable(t))
          TypeVar(renamed(t.symbol()))
        else
          t match {
            // ex: scala.List[int]
            case Type$TypeRef(_, sym, args) =>
              AtomicType(new TC(sym), listOfArray(args) map trans_atomic)
            // [a_1, ..., a_n] T
            case Type$PolyType(tparams, result) => trans(result)
            // (T_1, ..., T_n) => T
            case Type$MethodType(vparams, result) =>
              FunType(mkProd(listOfArray(vparams) map { x => trans(x.getType()) }), trans(result))
            // Types that do not have ML equivalents.
            case _ => throw TranslateExc
          }
      def trans_atomic(t: Type): MLType =
        if (is_localVariable(t))
          TypeVar(renamed(t.symbol()))
        else
          t match {
            // ex: scala.List[int]
            case Type$TypeRef(_, sym, args) =>
                AtomicType(new TC(sym), listOfArray(args) map trans_atomic)
            case _ => throw TranslateExc
          }
      try {
        val ml_t = trans(t);
        Some(Pair(ml_t, renaming))
      }
      catch {
        case e => {
          //          Console.println(e.toString());
          None
        }
      }
    }

    def translateAndPrint(t: Type): String = translate(t).toString();

    def isMLType(t: Type): boolean = translate(t) match {
      case None => false
      case _ => true
    }

    def arrayOfList(l: List[Symbol]): Array[Symbol] = {
      val a = new Array[Symbol](l.length);
      (l foldLeft 0) { (i, x) => { a(i) = x; i + 1 } };
      a
    }

    def stringOfArray(a: Array[Symbol]): String = {
      val buf = new StringBuffer();
      for(val i <- List.range(0, a.length)) buf.append(a(i).toString() + ", ");
      buf.toString()
    }

    def foreach(a: Array[Symbol])(def f: Symbol => Symbol): Array[Symbol] = {
      val b = new Array[Symbol](a.length);
      for(val i <- List.range(0, a.length)) b(i) = f(a(i));
      b
    }

    def subst(t: Type, ren: Map[Symbol, Symbol]): Type = {
      val Pair(from, to) = List.unzip(ren.toList);
      val from_array = arrayOfList(from);
      val to_array = arrayOfList(to);
      def subst_aux(t: Type): Type = t match {
        case Type$PolyType(tparams, result) => {
          val substTparams = foreach(tparams) { sym => ren(sym) };
          new Type$PolyType(substTparams, subst_aux(result))
        }
        case _ => t.subst(from_array, to_array)
      }
      val substType = subst_aux(t);
      substType
    }

    def searchIso(search: Type): Option[Type => Option[Map[Symbol, Symbol]]] =
      translate(search) match {
        case None => None
        case Some(Pair(search_ml, ren_search)) => {
          val iso_search = iso(search_ml);
          def comp(found: Type): Option[Map[Symbol, Symbol]] = translate(found) match {
            case None => None
            case Some(Pair(found_ml, ren_found)) =>
              iso_search(found_ml) match {
                case Triple(None, _, _) => None
                case Triple(Some(ren_unif), ren_a, ren_b) => {
                  val ren_ml = build_renaming(ren_unif, ren_a, ren_b);
                  val ren = scalaRenaming(ren_ml, ren_search, ren_found);
                  Some(ren)
                }
              }
          }
          Some(comp)
        }
      }

    def inverseMap[a,b](map: Map[a,b]): Map[b,a] =
      (map foldLeft (ListMap.Empty: Map[b,a])) {
        (map_inv, p) => (map_inv + p._2 -> p._1)
      }

    def composeMap[a,b,c](map1: Map[a,b], map2: Map[b,c]): Map[a,c] =
      (map1 foldLeft (ListMap.Empty: Map[a,c])) {
        (map_inv, p) =>
          if (map2.contains(p._2))
            (map_inv + p._1 -> map2(p._2))
          else
            map_inv
      }

    /**
    * Build the renaming to represent the matching type with same
    * type variables as in the request type.
    */
    def scalaRenaming(ren_unif: Map[int, int], ren_search: Map[Symbol, int], ren_found: Map[Symbol, int]): Map[Symbol, Symbol] =
      composeMap(ren_found, composeMap(ren_unif, inverseMap(ren_search)));

    def mkTypeVar(sym: Symbol): Type =
      Type.typeRef(Type.localThisType, sym, Type.EMPTY_ARRAY);

    def addParamTypes(paramTypes: Array[Type], t: Type): Type = t match {
      case Type$PolyType(tparams, result) => new Type$PolyType(tparams, addParamTypes(paramTypes, result))
      case Type$MethodType(vparams, result) => new Type$MethodType(vparams, addParamTypes(paramTypes, result))
      case _ => global.definitions.FUNCTION_TYPE(paramTypes, t)
    }

    def addClassContext(clazz: Symbol, t: Type): Type = {
      val class_tparam_symbols: Array[Symbol] = clazz.typeParams();
      val class_tparams: Array[Type] = new Array[Type](class_tparam_symbols.length);
      for(val i <- List.range(0, class_tparams.length))
        class_tparams(i) = mkTypeVar(class_tparam_symbols(i));
      val class_type = Type.appliedType(clazz.typeConstructor(), class_tparams);
      val paramTypes = new Array[Type](1);
      paramTypes(0) = class_type;
      t match {
        case Type$PolyType(tparams, result) => {
          val all_tparams = new Array[Symbol](class_tparam_symbols.length + tparams.length);
          for(val i <- List.range(0, class_tparam_symbols.length)) all_tparams(i) = class_tparam_symbols(i);
          for(val i <- List.range(0, tparams.length)) all_tparams(i + class_tparam_symbols.length) = tparams(i);
          new Type$PolyType(all_tparams, addParamTypes(paramTypes, result))
        }
        case _ =>  new Type$PolyType(class_tparam_symbols, addParamTypes(paramTypes, t))
      }
    }

    /** Implement type isomorphism. */
    def searchType(t: Type, isDocumented: SymbolBooleanFunction): java.util.Iterator/*[Pair[Symbol, Type]]*/ = {
      val found = new java.util.LinkedList;
      searchIso(t) match {
        case None => {}
        case Some(test) => {
          val rootNode: node.T = node.make(isDocumented)(global.definitions.ROOT).head;
          def searchInModule(m: node.T): unit = m.members foreach {
            member =>
              if (member.symbol.isClass())
                searchInClass(member)
              else if (member.symbol.isTerm()) {
                test(member.symbol.getType()) match {
                  case None => {}
                  case Some(ren) => {
                    val adaptedType = subst(member.symbol.getType(), ren);
                    val result = new SearchResult(member.symbol,
                                                  adaptedType,
                                                  false,
                                                  null);

                    found.add(result)
                  }
                }
                if (member.symbol.isPackage() || member.symbol.isModule())
                  searchInModule(member)
              }
          }
          def searchInClass(m: node.T): unit =  m.members foreach {
            member =>
              if (member.symbol.isTerm()) {
                val extended_type = addClassContext(m.symbol, member.symbol.getType());
                test(extended_type) match {
                  case None => {}
                  case Some(ren) => {
                    val adaptedType = subst(member.symbol.getType(), ren);
                    val adaptedTparams = foreach(m.symbol.typeParams()) { sym => ren(sym) };
                    val result = new SearchResult(member.symbol,
                                                  adaptedType,
                                                  true,
                                                  adaptedTparams);
                    found.add(result)
                  }
                }
              }
          }
          searchInModule(rootNode);
        }
      };
      found.iterator()
    }
  }

  object node {

    def listOfArray[a](r: Array[a]): List[a] =
      List.fromArray(r, 0, r.length);

    /** Documentation node. */
    trait T {
      val symbol: Symbol;
      def members: List[T];
    }

    /** Build a node from a root symbol. */
    def make(isDocumented: SymbolBooleanFunction)(root: Symbol): List[T] =
      if (ScalaSearch.isRelevant(root) && isDocumented.apply(root))
        List(new T {
          val symbol = root;
          def members: List[T] = listOfArray(ScalaSearch.members(root, isDocumented)) flatMap make(isDocumented);
        })
      else
        List();
  }

}

  /*
  object stringML extends ML with Application {

    class StringTC(s: String) extends TypeConstructor[StringTC] {
      val name: String = s;
      def sameAs(that: StringTC): boolean = name.equals(that.name);
      override def toString(): String = name;
    }

    type TC = StringTC;

    val list = new TC("List");
    val t = (at(list, x(11)) * (x(11) -> x(12))) -> at(list, x(12));
    val u = (x(13) -> x(14)) -> (at(list, x(13)) -> at(list, x(14)));
    printIso(t, u);
  }
  */

