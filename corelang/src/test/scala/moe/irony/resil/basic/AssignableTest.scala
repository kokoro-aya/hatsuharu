package moe.irony.resil.basic

import moe.irony.resil.lang.{Resil, Typing, newEnvironment}
import moe.irony.resil.sig.{AList, B, BoolT, Components, Ctor, CtorPattern, Data, I, IntT, IntV, Letrec, ListPattern, RecordPattern, RslBlock, RslTypedVar, RslVar, S, Struct, SumDecl, TuplePattern, Variable}

class AssignableTest extends munit.FunSuite:

  object SimplePatternTest:

    test("eval1-eval") {
      val expr = Letrec(
        List(
          (RslVar("a"), Components(List(I(1), S("str")), 2))
        ),
        Variable("a")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval1-typing") {
      val expr = Letrec(
        List(
          (RslVar("a"), Components(List(I(1), S("str")), 2))
        ),
        Variable("a")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }


    test("eval2-eval") {
      val expr = Letrec(
        List(
          (TuplePattern(List(RslVar("a"), RslVar("b"))), Components(List(I(1), S("str")), 2))
        ),
        Variable("a")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval2-typing") {
      val expr = Letrec(
        List(
          (TuplePattern(List(RslVar("a"), RslVar("b"))), Components(List(I(1), S("str")), 2))
        ),
        Variable("a")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }


    test("eval3-eval") {
      val expr = Letrec(
        List(
          (TuplePattern(List(RslVar("a"), RslVar("b"))),
            Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
        ),
        Variable("b")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval3-typing") {
      val expr = Letrec(
        List(
          (TuplePattern(List(RslVar("a"), RslVar("b"))),
            Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
        ),
        Variable("b")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }



    test("eval4-eval") {
      val expr = Letrec(
        List(
          (TuplePattern(List(RslVar("a"), RslVar("b"), RslVar("c"))),
            Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
        ),
        Variable("b")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval4-typing") {
      val expr = Letrec(
        List(
          (TuplePattern(List(RslVar("a"), RslVar("b"), RslVar("c"))),
            Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
        ),
        Variable("b")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }


    test("eval5-eval") {
      val expr = Letrec(
        List(
          (ListPattern(List(RslVar("x"), RslVar("xs"))),
            AList(List(I(1), I(2), I(3), I(4))
              //          AList(List(I(1), Components(List(I(97), S("bar"), B(false))), 2))
            ))),
        Variable("x")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval5-typing") {
      val expr = Letrec(
        List(
          (ListPattern(List(RslVar("x"), RslVar("xs"))),
            AList(List(I(1), I(2), I(3), I(4))
            ))),
        Variable("x")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }

    test("eval6-eval") {
      val expr = Letrec(
        List(
          (ListPattern(List(RslVar("x"), RslVar("xs"))),
            AList(List(I(1), Components(List(I(97), S("bar"), B(false)), 2))
            ))),
        Variable("x")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval6-typing") {
      val expr = Letrec(
        List(
          (ListPattern(List(RslVar("x"), RslVar("xs"))),
            AList(List(I(1), Components(List(I(97), S("bar"), B(false)), 2))
            ))),
        Variable("x")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }

    test("eval7-eval") {
      val expr = Letrec(
        List(
          (RecordPattern(List(
            RslVar("a"), RslVar("b"), RslVar("c")
          )),
            Struct(None, Map("a" -> I(3), "b" -> I(4), "c" -> B(false))))
        ),
        Variable("c")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval7-typing") {
      val expr = Letrec(
        List(
          (RecordPattern(List(
            RslVar("a"), RslVar("b"), RslVar("c")
          )),
            Struct(None, Map("a" -> I(3), "b" -> I(4), "c" -> B(false))))
        ),
        Variable("c")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }

  object TypedPatternTest:

    test("eval1-t-eval") {
      val expr1 = Letrec(
        List(
          (TuplePattern(List(
            RslVar("a"), RslVar("b"), RslTypedVar(RslVar("c"), BoolT))
          ),
            Components(List(I(3), I(4), I(3)), 3))
        ),
        Variable("c")
      )

      assertEquals(Resil().eval(expr1), IntV(1))
    }

    test("eval1-t-typing") {
      val expr1 = Letrec(
        List(
          (TuplePattern(List(
            RslVar("a"), RslVar("b"), RslTypedVar(RslVar("c"), BoolT))
          ),
            Components(List(I(3), I(4), I(3)), 3))
        ),
        Variable("c")
      )

      assertEquals(Typing().typecheck(expr1), Right(IntT))
    }
  
  object CompoundPatternTest:

    test("eval8-eval") {
      val expr = Letrec(
        List(
          (TuplePattern(List(ListPattern(List(RslVar("x"), RslVar("xs"))), RslVar("ys"))),
            Components(
              List(
                (AList(List(I(1), I(2), I(3), I(4)))),
                (AList(List(I(1), I(2), I(3), I(4))))
              ), 2))),
        Variable("ys")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval8-typing") {
      val expr = Letrec(
        List(
          (TuplePattern(List(ListPattern(List(RslVar("x"), RslVar("xs"))), RslVar("ys"))),
            Components(
              List(
                (AList(List(I(1), I(2), I(3), I(4)))),
                (AList(List(I(1), I(2), I(3), I(4))))
              ), 2))),
        Variable("ys")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }


    test("eval9-error-eval") {
      val expr = Letrec(
        List(
          (TuplePattern(List(ListPattern(List(RslVar("x"), RslVar("xs"))), RslVar("ys"))),
            Components(
              List(
                (AList(List(I(1), I(2), I(3), S("S")))),
                (AList(List(I(1), I(2), I(3)))),
                (AList(List(S("S"), S("Z"))))
              ), 2))),
        Variable("ys")
      )

      assertEquals(Resil().eval(expr), IntV(1))
    }

    test("eval9-error-typing") {
      val expr = Letrec(
        List(
          (TuplePattern(List(ListPattern(List(RslVar("x"), RslVar("xs"))), RslVar("ys"))),
            Components(
              List(
                (AList(List(I(1), I(2), I(3), S("S")))),
                (AList(List(I(1), I(2), I(3)))),
                (AList(List(S("S"), S("Z"))))
              ), 2))),
        Variable("ys")
      )

      assertEquals(Typing().typecheck(expr), Right(IntT))
    }

  object ADTPatternTest:

    test("eval10-eval") {
      val blocks1 =
        List[RslBlock](
          SumDecl(
            "Shape", List(), List(
              Ctor("Square", List("side" -> IntT)),
              Ctor("Circle", List("radius" -> IntT)),
              Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
            )),
          Letrec(
            List(
              (CtorPattern("Rectangle", List(
                RslVar("width"), RslVar("height")
              )),
                Data("Rectangle", List(I(15), I(7))))
            ),
            Variable("width")
          )
        )

      val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks1)

      res1.foreach { it =>
        assertEquals(it, IntV(1))
      }
    }

    test("eval10-typing") {
      val blocks1 =
        List[RslBlock](
          SumDecl(
            "Shape", List(), List(
              Ctor("Square", List("side" -> IntT)),
              Ctor("Circle", List("radius" -> IntT)),
              Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
            )),
          Letrec(
            List(
              (CtorPattern("Rectangle", List(
                RslVar("width"), RslVar("height")
              )),
                Data("Rectangle", List(I(15), I(7))))
            ),
            Variable("width")
          )
        )

      assertEquals(Typing().typecheck(blocks1), Right(List(IntT)))
    }