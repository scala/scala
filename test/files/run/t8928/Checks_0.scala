package test

import reflect.api.Universe

class Checks[U <: Universe with Singleton](val universe: U, ordered: Boolean) {
  import universe._

  def check(tpe: Type): Unit = {
    val ObjectTpe = typeOf[Object]

    def assertMatch(name: String, v: Tree)(pf: PartialFunction[Tree, Any]): Unit =
      if (!pf.isDefinedAt(v)) throw new AssertionError(s"$name: ${showRaw(v)}")

    val anns: List[Annotation] = tpe.typeSymbol.annotations

    anns match {
      case List(noArgs, simple, nested, arrays, enum, empty) =>
        assert(noArgs.tree.tpe =:= typeOf[NoArgs_0])
        assert(noArgs.tree.children.size == 1)

        assert(simple.tree.tpe =:= typeOf[Simple_0])

        val parameters: List[(String, Tree)] =
          simple.tree.children.tail.map {
            case AssignOrNamedArg(Ident(TermName(name)), value) =>
              name -> value
          }

        /* Runtime reflection does not preserve ordering on java annotations,
         * so check only when we're in a macro universe. */
        if (ordered) {
          assert(parameters.map(_._1) == List(
            "_byte", "_char", "_short", "_int", "_long", "_float", "_double", "_string", "_class"))
        }

        val List(
          ("_byte", byte), ("_char", char), ("_class", clasz), ("_double", double),
          ("_float", float), ("_int", int), ("_long", long), ("_short", short), ("_string", string)
        ) = parameters.sortBy(_._1)

        assertMatch("byte", byte)     { case Literal(Constant(1))         =>  }
        assertMatch("char", char)     { case Literal(Constant('2'))       =>  }
        assertMatch("short", short)   { case Literal(Constant(3))         =>  }
        assertMatch("int", int)       { case Literal(Constant(4))         =>  }
        assertMatch("long", long)     { case Literal(Constant(5L))        =>  }
        assertMatch("float", float)   { case Literal(Constant(6.7f))      =>  }
        assertMatch("double", double) { case Literal(Constant(8.9d))      =>  }
        assertMatch("string", string) { case Literal(Constant("ten"))     =>  }
        assertMatch("class", clasz)   { case Literal(Constant(ObjectTpe)) =>  }


        assert(nested.tree.tpe =:= typeOf[Nested_0])
        nested.tree.children match {
          case _ :: inner :: Nil =>
            assertMatch("inner", inner) {
              case AssignOrNamedArg(Ident(TermName("inner")), Apply(Select(New(tpe), nme.CONSTRUCTOR), AssignOrNamedArg(Ident(TermName("value")), Literal(Constant("turkey"))) :: Nil))
                if tpe.tpe =:= typeOf[Nested_0.Inner] =>
            }
        }

        assert(arrays.tree.tpe =:= typeOf[Array_0.Repeated])
        arrays.tree.children match {
          case _ :: AssignOrNamedArg(Ident(TermName("value")), Apply(arr, fst :: snd :: Nil)) :: Nil =>
            assertMatch("value(0)", fst) {
              case Apply(Select(New(tpe), nme.CONSTRUCTOR), AssignOrNamedArg(Ident(TermName("value")), Apply(arr, args)) :: Nil)
                if ((args zip Seq(8, 6, 7, 5, 3, 0, 9)) forall { case (Literal(Constant(l)), r) => l == r }) &&
                  tpe.tpe =:= typeOf[Array_0] =>
            }
            assertMatch("value(1)", snd) {
              case Apply(Select(New(tpe), nme.CONSTRUCTOR), AssignOrNamedArg(Ident(TermName("value")), Apply(arr, args)) :: Nil)
                if ((args zip Seq(6)) forall { case (Literal(Constant(l)), r) => l == r }) &&
                  tpe.tpe =:= typeOf[Array_0] =>
            }
        }
        assert(enum.tree.tpe =:= typeOf[Enum_0])

        assert(empty.tree.tpe =:= typeOf[Empty_0])
        assert(empty.tree.children.size == 1)
    }
  }

}