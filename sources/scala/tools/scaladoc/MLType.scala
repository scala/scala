import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.symtab.Definitions;
import scalac.Global;

package scala.tools.scaladoc {

  import scala.collection.immutable._;

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
      def flatten(t: MLType):Type_coords = t match {
        case ProdType(l,r) => {
          val Pair(len1, l1) = flatten(l);
          val Pair(len2, l2) = flatten(r);
          Pair(len1 + len2, l1 ::: l2)
        }
        case _ => Pair(1, List(t))
      }
      flatten(normalForm(t));
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

    def split_vars(start: int, t: Type_coords): Pair[Pair[int, Renaming], Type_coords] = {
      /**
      * Take a coordinate and return a copy with type variables renamed and
      * keeping track of the renaming.
      */
      def shift_compact_vars(p: Pair[int, Renaming], t: MLType) : Pair[Pair[int, Renaming], MLType] = {
        val Pair(start, ren) = p;
        t match{
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

  class ScalaML(global: Global) extends ML {

    class ScalaTC(sym: Symbol) extends TypeConstructor[ScalaTC] {
      val symbol: Symbol = sym;
      def sameAs(that: ScalaTC): boolean = (symbol == that.symbol);
      override def toString(): String = symbol.nameString();
    }

    type TC = ScalaTC;

    def mkProd(l: List[MLType]): MLType = l match {
      case List() => UnitType
      case t :: List() => t
      case t :: ts => ProdType(t, mkProd(ts))
    }

    def isUnit(t: Type) = t.isSameAs(global.definitions.UNIT_TYPE());

    case object TranslateExc extends Exception;

//     def translate(t: Type): Option[MLType] = {
//       def trans(env: Map[Symbol, int], t: Type): Option[MLType] = {
//         t match {
//           // scala.Unit
//           case Type$TypeRef(pre, sym, args) if (isUnit(t)) =>

//           // scala.Tuple_n[T_1, ..., T_n]
//           case Type$TypeRef(pre, sym, args) => ProdType(, )

//           // scala.Function_n[T_1, ..., T_n, T]
//           case Type$TypeRef(pre, sym, args) => FunType(, )

//           // a (type parameter)
//           case Type$TypeRef(pre, sym, args)
//           if (sym.isAbstractType() && sym.isParameter()) => TypeVar(env(sym))

//           // (T_1, ..., T_n) => T
//           case Type$MethodType(vparams, result) =>
//             FunType(mkProd(vparams map { x => trans(x.getType()) }), trans(result))

//           // [a_1, ..., a_n] T
//           case Type$PolyType(tparams, result) =>  trans(result)
//           case _ => throw TranslateExc
//         }
//         try {
//           Some(trans(ListMap.Empty, t))
//         }
//         catch {
//           case _ => None
//         }
//     }

  }

  object stringML extends ML with Application {

    class StringTC(s: String) extends TypeConstructor[StringTC] {
      val name: String = s;
      def sameAs(that: StringTC): boolean = name.equals(that.name);
      override def toString(): String = name;
    }

    type TC = StringTC;

    val list = new TC("List");
//     val t = unt -> (x(1) * at(list, x(2))) -> (x(3) * x(4) * unt);
//     Console.println(t.toString());
//     Console.println(normalForm(t).toString());
//     Console.println(splitRewrite(0, t).toString());
//     val u = t;
//     Console.println(iso(unt)(unt).toString());
//     Console.println(iso(t)(u).toString());
//     Console.println(iso(t)(normalForm(t)).toString());
//     val f1 = x(1);
//     val f2 = x(1) -> x(2);
//     val f3 = x(1) -> (x(2) -> x(3));
//     Console.println(f1.toString());
//     Console.println(flatten(f1).toString());
//     Console.println(f2.toString());
//     Console.println(flatten(f2).toString());
//     Console.println(f3.toString());
//     Console.println(flatten(f3).toString());
//     Console.println(perms(List(1,2,3,4)).toString());
    val t = (at(list, x(11)) * (x(11) -> x(12))) -> at(list, x(12));
    val u = (x(13) -> x(14)) -> (at(list, x(13)) -> at(list, x(14)));
    printIso(t, u);
  }
}

