package scala.runtime.matching ;
/** contexts fully determined which derivation was used along a path,
*   thereby allowing to select the proceed the %next% derivations.
*
*   we use here that derivations are ordered.
*/
abstract class PathContext with Ordered[PathContext] {
  val len = 0;
  def isCompatible( other:PathContext ):boolean ;
  def ~( other:PathContext ):boolean ;
  def prune( len:int ):PathContext = this;
  def address:List[Int] = 0::Nil;
  def toString1():String = "";

  def compareTo [b >: PathContext <% Ordered[b]](that: b): int = that match {
    case t:PathContext =>
      if( rctx_inf(this,t) )
        -1
      else if( rctx_eq(this,t) )
        0
      else
        1;
    case _ => -(that compareTo this)
  }



  final def rctx_inf( x:PathContext, y:PathContext ):boolean = x match {
    case EmptyContext => y match {
      case EmptyContext => false;
      case _ => true;
    }
    case RuleContext(r1, up1) => y match {
      case RuleContext(r2,up2) =>
        ((r1 == r2 ) && rctx_inf(up1, up2))
        || ( r1 < r2 )
      case _ => false;
    }
  }

  final def rctx_eq( x:PathContext, y:PathContext ) = x == y;
}
/** use HedgeNT H */
/*
case class HedgeContext( H:HedgeNT, outer:PathContext ) extends PathContext {
  override def toString():String = outer.toString()+":"+H.toString();
def isCompatible( other:PathContext ):boolean   = error("don't call me");
def ~( other:PathContext )   = error("don't call me");
}
*/
/** use TreeNT T */
/*
case class TreeContext( T:TreeNT, up:PathContext ) extends PathContext {
  override def toString():String = up.toString()+":"+T.toString();
def isCompatible( other:PathContext ):boolean = error("don't call me");
def ~( other:PathContext )  = error("don't call me");
};
*/
/** the root context */
case object EmptyContext extends PathContext {
  override def toString1() = "()";
  override def toString():String = "()";
  def isCompatible( other:PathContext ):boolean = true;
  override def ~( other:PathContext ) = true;
}

/* inv: up is RuleContext || EmptyContext */
case class RuleContext( r:Rule, up:PathContext ) extends PathContext {

  override def toString1():String = { up.toString1()+";"+r.toString() }

  override def toString():String = toString1()+"["+address.reverse+"]";
  override val len = up.len + 1;

  def down(li:List[Int]):List[Int] = 0::li;

  def right(li:List[Int]):List[Int] = li match {
    case i::is => i+1::is
  }

  override def address:List[Int] = r match {
    case HedgeRule( _, _ ) => right( up.address );
    case AnyNodeRule( _, _ ) => down( up.address );
    case TreeRule( _, _ ) =>  down( up.address );
    case AnyTreeRule( _ ) =>  down( up.address );
  }

  /* if have common prefix */
  override def isCompatible( other:PathContext ):boolean = {
    Console.println( "isCompatible called\nthis:"+this+" len="+this.len+"\nother"+other+" len = "+other.len);
    val q = {
      if( this.len > other.len ) {
        Console.println("this.prune(otherlen) = "+ this.prune( other.len ));

        val tmp = this.prune( other.len );
        ( tmp == other )||( tmp ~ other )

      } else if( this.len < other.len ) {
        val tmp = other.prune( this.len );
        Console.println("other.prune(thislen) = "+ tmp);
        ( this == tmp )||( this ~ tmp );
      } else
        (this == other )|| this ~ other
      // ^ for x @ y @ p
    };
    Console.println("result: "+q);
    q
  }

  override def ~( other:PathContext ) = {
    def matchesH( H:HedgeNT, r2:Rule ) = r2 match {
      case HedgeRule( H, _ ) => true
      case _ => false;
    };
    def matchesT( T:TreeNT, r2:Rule ) = r2 match {
      case TreeRule( T, _ ) => true
      case AnyTreeRule( T ) => true
      case AnyNodeRule( T, _ ) => true
      case _ => false;
    };
    (other match {
      case RuleContext( r2, up2 ) =>
      ( (up == up2) && (this.up match {

        case RuleContext(HedgeRule(h, Pair(t1,h1)),_) =>
          ( matchesT( t1, this.r ) && matchesH( h1, r2 ) )
          ||( matchesT( t1, r2 ) && matchesH( h1, this.r ) )

        case _ => false;

      })  || up ~ up2 )

        case EmptyContext => true;/*other match {
          case EmptyContext => true;
          case _ => false;
        }*/
    })
  }

  override def prune( len:int ):PathContext = {
    if( this.len > len ) up.prune( len ) else this;
  }
}

/*
def ctx_inf( x:PathContext, y:PathContext ):boolean = {
  x match {
    case EmptyContext => y match {
      case EmptyContext => false;
case _ => true;
    }
case HedgeContext( h:HedgeNT, out:PathContext ) => y match {
  case HedgeContext( h2:HedgeNT, out2:PathContext ) =>
    (ctx_eq(out,out2) &&( h.i < h2.i )) || ctx_inf( out,out2 )
case _ => false
}
case TreeContext( t:TreeNT, up:PathContext ) => y match {
  case TreeContext( t2:TreeNT, up2:PathContext ) =>
    (ctx_eq(up,up2) && (t.i < t2.i)) || ctx_inf( up, up2 )
case _ => true
}
  }
}

def ctx_eq( x:PathContext, y:PathContext ):boolean = {
  x match {
    case EmptyContext => y match {
      case EmptyContext => true;
case _ => true;
    }
case HedgeContext( h:HedgeNT, out:PathContext ) => y match {
  case HedgeContext( h2:HedgeNT, out2:PathContext ) =>
    ctx_eq(out,out2) &&( h.i == h2.i )
case _ => false
}
case TreeContext( t:TreeNT, up:PathContext ) => y match {
  case TreeContext( t2:TreeNT, up2:PathContext ) =>
    ctx_eq(up,up2) && (t.i == t2.i)
case _ => false
}
  }
}
*/
//val contextOrder = new Order[PathContext]( ctx_inf, ctx_eq ) ;

//  val ruleContextOrder = new Order[PathContext]( rctx_inf, rctx_eq ) ;


/* matcher needs this operations
isApplicableTree(1):
  given string s, and set of TreeNTs A, return a A'(\subseteq A) and a set B'
  such that for every t' in A' grammar has a rule t' -> s < h' >

maybe grammar is a map Str -> Pow(P x Q) rather than P -> Str x Q or
array P x Str_Index -> Q (use hashmap for strings that appear in a pattern!)
(P = treents, Q = hedgeNTs) and a map Q x P -> Q rather than Q -> P x Q

isApplicableTree(2):
  given a set B'' ( subseteq B' ) of hedgeNTs, and s and A' like above,
  return A''(\subseteq A') with those rules that have nonterminals from B''.

see above

isApplicableHedge(1)
  get epsilon closure of hedgeNTs (followChainRules) ?? needed ??

isApplicableHedge(2)
  given set B of hedgeNTs h, return a set A of treeNTs t such that
  there is rule h -> ( t , _ )

isApplicableHedge(3)
  given set B and set A'(subseteq A), return a set C of hedgeNTs h2 such that
  there is rule h -> ( t' , h2 )

isApplicableHedge(4)
  given set C' (\subseteq C)  extract B' (subseteq B) such that there is
  rule h' -> ( _ , h2' )

*/
