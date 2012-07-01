
package scala.reflect.dynamic

/** A dynamic wrapper class that passes calls through to a target object.
 *
 *
 *  Any attempt to access a method or field of this class will be
 *  passed through to the target object through reflection. The result
 *  will always be case to type `Any`. No compile-time checks are
 *  performed. Rather, if an invalid access attempt is made (e.g. attempting
 *  to access a member that does not exist), a runtime exception will be raised.
 *
 *  By overriding `allowPrivate`, you can also create a convenient proxy that allows
 *  access to an object's private members without requiring you to write a lot of
 *  reflective code. 
 *
 *  @example {{{
 *  import reflect.dynamic.DynamicProxy
 *  // Define an object to wrap
 *  object O {
 *    def foo(x: Int, y: Int) = x + y
 *  }
 *
 *  // Create a proxy
 *  val proxy = new DynamicProxy {
 *    val proxyTarget = O
 *  }
 *
 *  // Call the method through the proxy
 *  val result = proxy.foo(1,4)
 *
 *  if (result == 5) println("Expected Result")
 *  else println("Weird Result")
 *  
 *  }}}
 *
 *  @example {{{
 *  import reflect.dynamic.DynamicProxy
 *
 *  // Again, create an object to wrap
 *  object O {
 *    private def foo(x: Int, y: Int) = x + y
 *  }
 *
 *  // This time, we will override allowPrivate
 *  val proxy = new DynamicProxy {
 *    val proxyTarget = O
 *    override val allowPrivate = true
 *  }
 *  
 *  val result = proxy.foo(5,6)
 *
 *  if (result == 11) println("We Accessed a Private Method!")
 *  else println("Weird Result")
 *
 *  }}}
 *
 *  @author Christopher Hodapp
 *  @version 2.10, 29/6/2012
 *  @since 2.10
 *
 */

trait DynamicProxy extends Dynamic {

  import language.implicitConversions
  import collection.mutable
  import Seq.{ empty => snil }
  import reflect.runtime.{universe => u}
  import u.definitions._


  /** This is the target of the proxy.
   *  An implementation must override this with the object that the proxy should
   *  pass calls through to.
   */
  val proxyTarget: Any

  /** This controls whether to allow references to resolve private members.
   *
   * If you override this with true, you can create a proxy that ignores the
   * public/private distinction, allowing you to access private members through the
   * normal syntax.
   */
  def allowPrivate = false

  private[this] lazy val m   = u.runtimeMirror(proxyTarget.getClass.getClassLoader)
  private[this] lazy val im  = m.reflect(proxyTarget)
  private[this] lazy val sym = im.symbol
  private[this] lazy val tpe = sym.asType

  /** Used to capture the types of arguments along with their values.
   *
   *  This class's existance is an implementation detail and you
   *  should never have to create instances of it yourself.
   * 
   * @param tpe the type of the value
   * @param value the actual value
   */
  case class Boxed(tpe: u.Type, value: Any)

  /** Companion for class `Boxed` used to house implicit conversions from any object
   * to a Boxed instance.
   */
  object Boxed {
    /** implicit conversion from any type to a `Boxed` instance.
     * @param v the object to be boxed\
     * @return the a `Boxed` instance where the `value` is `v` and the `tpe` is `v`'s type
     */
    implicit def box[@specialized T: u.TypeTag](v: T) = Boxed(u.typeOf[T], v)
  }

  /** This method is a proxy for selections. A selection is a nullary member reference (i.e.
   *  an access of a field or a call to a method that doesn't take arguments).
   *
   *  You should not have to call this method directly in normal use of `DynamicProxy`.
   */
  def selectDynamic(name: String): Any = invoke(name)(snil)(snil)(snil)

  /** This method is a proxy for updates. An update is an assignment reference (e.g. setting
   * a field equal to a value).
   *
   * You should not have to call this method directly in normal use of `DynamicProxy`.
   */
  def updateDynamic(name: String)(value: Boxed): Any = {
    val realName = u.nme.getterToSetter(u.newTermName(name)).encoded
    invoke(realName)(snil)(Seq((value.tpe, value.value)))(snil)
  }

  /** This method is a proxy for applications. An application is a function call reference (e.g.
   *  calling a member method).
   *
   *  You should not have to call this method directly in normal use of `DynamicProxy`.
   */
  def applyDynamic(name: String)(args: Boxed*): Any =
    invoke(name)(snil)(args.map(arg => (arg.tpe, arg.value)))(snil)

  /** This method is a proxy for named applications. A named application is a function call
   *  reference that names the parameters for its arguments (e.g. calling a member method with
   *  named-parameter arguments).
   *
   *  You should not have to call this method directly in normal use of `DynamicProxy`.
   */
  def applyDynamicNamed(name: String)(args: (String, Boxed)*): Any = {
    val (positional, named) = args.span(_._1.isEmpty)
    val p = positional.map { x => val arg = x._2; (arg.tpe, arg.value) }
    val n = named.map { x => val arg = x._2; (x._1, arg.tpe, arg.value) }
    invoke(name)(snil)(p)(n)
  }

  private [this] def invoke
    (name: String)
    (posTargs: Seq[u.Type])
    (posVargs: Seq[(u.Type, Any)])
    (nameVargs: Seq[(String, u.Type, Any)]): Any = {

      val pta = posTargs
      val pva = posVargs
      val nva = nameVargs
      val helpers =
        new ConcreteReflectionHelpers
          with ConcreteCorrelationHelpers
          with ConcreteSelectionHelpers
          with KnowsArgs {

          val posTargs = pta
          val posVargs = pva
          val nameVargs = nva
      }
      
      import helpers._

      val sameName =
        if (allowPrivate) tpe.member(u.newTermName(name).encodedName).asTermSymbol.alternatives.map(_.asMethodSymbol)
        else tpe.nonPrivateMember(u.newTermName(name).encodedName).asTermSymbol.alternatives.map(_.asMethodSymbol)

      val selection = select(sameName, defaultFilteringOps)

      if (selection.length == 1) {
        val method = im.reflectMethod(selection.head)
        val arguments = fixArguments(selection.head).get.map(_._2)
        val evalDefaults = arguments.map {
          case Left(a) => a
          case Right(d) => im.reflectMethod(d).apply()
        }
        method.apply(evalDefaults.toSeq: _*)
      } else if (selection.length > 1) {
        sys.error("Ambiguous Reference")
      } else if (selection.length < 1) {
        sys.error("No Such Method")
      }
  }

  private[this] trait ReflectionHelpers {
    def typeParams(x: u.Type): Seq[u.Type]
    def paramTypes(method: u.MethodSymbol): Seq[u.Type]
    def defaultValues(method: u.MethodSymbol): Map[Int, u.MethodSymbol]
    def valueParams(x: u.MethodSymbol): Seq[u.TermSymbol]
  }

  private[this] trait CorrelationHelpers {
    def shape(arg: u.Type): u.Type
    def signatureAsSpecific(method1: u.MethodSymbol, method2: u.MethodSymbol): Boolean
    def scopeMoreSpecific(method1: u.MethodSymbol, method2: u.MethodSymbol): Boolean
    def moreSpecific(method1: u.MethodSymbol, method2: u.MethodSymbol): Boolean
    def fixArguments(method: u.MethodSymbol): Option[Seq[(u.Type, Either[Any, u.MethodSymbol])]]
  }

  private[this] trait SelectionHelpers {
    def defaultFilteringOps: Seq[Seq[u.MethodSymbol] => Seq[u.MethodSymbol]]
    def select
      (
        alternatives: Seq[u.MethodSymbol],
        filters: Seq[Seq[u.MethodSymbol] => Seq[u.MethodSymbol]]
      ): Seq[u.MethodSymbol]
  }

  private[this] trait KnowsArgs {
    def posTargs: Seq[u.Type]
    def posVargs: Seq[(u.Type, Any)]
    def nameVargs: Seq[(String, u.Type, Any)]
  }

  private[this] trait ConcreteReflectionHelpers extends ReflectionHelpers {

    def paramTypes(method: u.MethodSymbol): Seq[u.Type] = {
      val symbols = method.typeSignature match {
        case u.MethodType(params, _) => params
        case u.NullaryMethodType(_) => Seq.empty
      }
      symbols.map(_.typeSignature)
    }

    def defaultValues(method: u.MethodSymbol): Map[Int, u.MethodSymbol] = {
      valueParams(method).zipWithIndex.filter(_._1.hasFlag(u.Flag.DEFAULTPARAM)).map { case(_, index) =>
        val name = u.nme.defaultGetterName(method.name.decodedName, index + 1)
        val tpe = method.owner.typeSignature
        val default =
          if (allowPrivate) tpe.member(name)
          else tpe.nonPrivateMember(name)
        index -> default.asMethodSymbol
      }.toMap
    }

    def typeParams(x: u.Type): Seq[u.Type] =
      x.typeConstructor.typeParams.map(_.asTypeSymbol.asType)

    def valueParams(method: u.MethodSymbol): Seq[u.TermSymbol] = {
      val symbols = method.typeSignature match {
        case u.MethodType(params, _) => params
        case u.NullaryMethodType(_) => Seq.empty
      }
      symbols.map(_.asTermSymbol)
    }

  }
  
  private[this] trait ConcreteCorrelationHelpers extends CorrelationHelpers {
    self: ReflectionHelpers with KnowsArgs =>

    class Target

    def shape(arg: u.Type): u.Type = {

      val functionTypes =
        0 to 22 map (x => u.appliedType(FunctionClass(x).asType, (1 to x map (_ => AnyClass.asType)).toList :+ u.typeOf[Target]))

      val argErasure = arg.erasure
      val functionShape = functionTypes.find(argErasure <:< _.erasure)

      functionShape.map { fs =>
        val returnShape = shape(arg.typeArguments.last)
        fs.map(t => if (t =:= u.typeOf[Target]) returnShape else t)
      }.getOrElse(u.typeOf[Nothing])

    }

    def signatureAsSpecific(method1: u.MethodSymbol, method2: u.MethodSymbol): Boolean = {
      (method1.typeSignature, method2.typeSignature) match {
        case (u.NullaryMethodType(r1), u.NullaryMethodType(r2)) =>
          r1 <:< r2
        case (u.NullaryMethodType(_), u.MethodType(_, _)) =>
          true
        case (u.MethodType(_, _), u.NullaryMethodType(_)) =>
          false
        case (u.MethodType(p1, _), u.MethodType(p2, _)) =>
          (p1, p2).zipped.forall { (left, right) =>
            left.typeSignature <:< right.typeSignature
          }
      }
    }

    def scopeMoreSpecific(method1: u.MethodSymbol, method2: u.MethodSymbol): Boolean = {
      val o1 = method1.owner.asClassSymbol
      val o2 = method2.owner.asClassSymbol
      val c1 = if (o1.hasFlag(u.Flag.MODULE)) o1.companionSymbol else o1
      val c2 = if (o2.hasFlag(u.Flag.MODULE)) o2.companionSymbol else o2
      c1.typeSignature <:< c2.typeSignature
    }

    def moreSpecific(method1: u.MethodSymbol, method2: u.MethodSymbol): Boolean = {
      def points(m1: u.MethodSymbol, m2: u.MethodSymbol) = {
        val p1 = if (signatureAsSpecific(m1, m2)) 1 else 0
        val p2 = if (scopeMoreSpecific(m1, m2)) 1 else 0
        p1 + p2
      }
      points(method1, method2) > points(method2, method1)
    }

    def fixArguments(method: u.MethodSymbol): Option[Seq[(u.Type, Either[Any, u.MethodSymbol])]] = {
      val defaults = defaultValues(method)
      val base = posVargs.map(x => Some((x._1, Left(x._2))))
      val params = valueParams(method)
      val rest = {
        val offset = base.length
        val unfilledParams = params.zipWithIndex.drop(offset)
        val canAcceptAllArgs = nameVargs.forall { a =>
          unfilledParams.exists(_._1.name == u.newTermName(a._1))
        }
        if (canAcceptAllArgs) {
          unfilledParams.map { case (param, index) =>
            val matchingArgument =
              nameVargs.find(arg => u.newTermName(arg._1).encodedName == param.name.encodedName)
            matchingArgument.map { case (name, tpe, value) =>
              Some((tpe, Left(value)))
            }.getOrElse {
              if (param.hasFlag(u.Flag.DEFAULTPARAM)) {
                defaults.get(index).map { choice =>
                  val tpe = choice.typeSignature match {
                    case u.MethodType(_, resultType) => resultType
                    case u.NullaryMethodType(resultType) => resultType
                  }
                  (tpe, Right(choice))
                }
              }
              else None
            }
          }
        } else Seq(None)
      }
      val maybe = base ++ rest
      val flat = maybe.flatten
      if (flat.length == params.length) Some(flat)
      else None
    }
    
  }

  private[this] trait ConcreteSelectionHelpers extends SelectionHelpers {
    self: ReflectionHelpers with ConcreteCorrelationHelpers with KnowsArgs =>

    def select (
        alternatives: Seq[u.MethodSymbol],
        filters: Seq[Seq[u.MethodSymbol] => Seq[u.MethodSymbol]]
    ): Seq[u.MethodSymbol] =
      filters.foldLeft(alternatives)((a, f) => if (a.tail nonEmpty) f(a) else a)

    val posTargLength: Seq[u.MethodSymbol] => Seq[u.MethodSymbol] =
      _.filter(a => a.typeSignature.typeParams.length == posTargs.length)

    def defaultFilteringOps =
      Seq(posTargLength, shapeApplicable, applicable, noDefaults, mostSpecific)

    val shapeApplicable: Seq[u.MethodSymbol] => Seq[u.MethodSymbol] = _.filter{ a =>
      fixArguments(a) exists { args =>
        val tpes = args.map(_._1)
        val shapes = tpes.map(shape)
        (shapes, paramTypes(a)).zipped.forall(_ <:< _)
      }
    }

    val applicable: Seq[u.MethodSymbol] => Seq[u.MethodSymbol] = _.filter { a =>
      fixArguments(a) exists { args =>
        val tpes = args.map(_._1)
        (tpes, paramTypes(a)).zipped.forall(_ <:< _)
      }
    }

    val noDefaults: Seq[u.MethodSymbol] => Seq[u.MethodSymbol] =
      _.filter(a => defaultValues(a).size == 0)

    val mostSpecific: Seq[u.MethodSymbol] => Seq[u.MethodSymbol] = { alts =>
      val sorted = alts.sortWith(moreSpecific)
      val mostSpecific = sorted.head
      val disagreeTest: u.MethodSymbol => Boolean =
        moreSpecific(_, mostSpecific)
      if (sorted.tail.exists(disagreeTest)) {
        mostSpecific +: sorted.tail.filter(disagreeTest)
      } else {
        Seq(mostSpecific)
      }
    }
  }

}
