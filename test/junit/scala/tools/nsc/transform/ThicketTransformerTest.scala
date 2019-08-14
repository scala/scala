package scala.tools.nsc.transform

import org.junit.Assert.assertSame
import org.junit.{Assert, Test}

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.Settings

class ThicketTransformerTest {
  @Test def thicketExpansion(): Unit = {
    val g = new scala.tools.nsc.Global(new StoreReporter(new Settings))
    g.settings.usejavacp.value = true
    new g.Run
    import g._
    object testTransformers extends TypingTransformers {
      val global: g.type = g
      val dummyUnit = newCompilationUnit("")

      object testTransformer extends ThicketTransformer(newRootLocalTyper(dummyUnit)) {
        override def transform(tree: Tree): Tree = tree match {
          case Literal(Constant(s: String)) if (s.toLowerCase() != s) => Literal(Constant(s.toLowerCase()))
          case Literal(Constant(s: String)) => s.toList.filterNot(_ == '-') match {
            case Nil => Thicket(Nil, EmptyTree)
            case a :: Nil => tree
            case as =>
              Thicket(Block(as.map(c => Literal(Constant(c.toString)).setType(definitions.StringTpe)): _*))
          }
          case t => super.transform(t)
        }
      }
    }
    def t(t: Tree): Tree = testTransformers.testTransformer.transform(t)
    def assertTreeEquals(expected: Tree, actual: Tree) = Assert.assertEquals(expected.toString, actual.toString)

    def s(s: String) = Literal(Constant(s))
    def i(i: Int) = Literal(Constant(i))

    locally {
      val noTransform = Block(s("a") :: Nil, s("b"))
      assertSame(noTransform, t(noTransform))
    }

    locally {
      val noTransform = Block(s("a") :: s("b") :: Nil, s("c"))
      assertSame(noTransform, t(noTransform))
    }

    locally {
      val transformStats = Block(s("ab") :: i(1) :: s("cd") :: Nil, s("e"))
      assertTreeEquals(Block(s("a") :: s("b") :: i(1) :: s("c") :: s("d") :: Nil, s("e")), t(transformStats))
    }

    locally {
      val transformStats = Block(s("ab") :: s("cd") :: Nil, s("e"))
      assertTreeEquals(Block(s("a") :: s("b") :: s("c") :: s("d") :: Nil, s("e")), t(transformStats))
    }

    locally {
      val transformExpr = Block(s("a") :: s("b") :: Nil, s("cd"))
      assertTreeEquals(Block(s("a") :: s("b") :: s("c") :: Nil, s("d")), t(transformExpr))
    }

    locally {
      val transformStatsExpr = Block(s("ab") :: s("cd") :: Nil, s("ef"))
      assertTreeEquals(Block(s("a") :: s("b") :: s("c") :: s("d") :: s("e") :: Nil, s("f")), t(transformStatsExpr))
    }

    locally {
      val transformStatsExpr = Block(s("A") :: s("B") :: Nil, s("cd"))
      assertTreeEquals(Block(s("a") :: s("b") :: s("c") :: Nil, s("d")), t(transformStatsExpr))
    }

    locally {
      val transformStatsExpr = Block(s("A") :: s("B") :: Nil, s("C"))
      assertTreeEquals(Block(s("a") :: s("b") :: Nil, s("c")), t(transformStatsExpr))
    }

    locally {
      val transformStatsExpr = Block(s("-") :: Nil, s("a"))
      assertTreeEquals(Block(Nil, s("a")), t(transformStatsExpr))
    }
  }
}
