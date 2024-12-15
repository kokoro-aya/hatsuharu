package moe.irony.resil.basic

import moe.irony.resil.lang.{Resil, Typing, newEnvironment}
import moe.irony.resil.sig.{AList, B, BoolT, BoolV, Components, Ctor, CtorPattern, Data, ErrV, I, IntT, IntV, Letrec, ListPattern, ListT, ListV, RecordPattern, RslBlock, RslTypedVar, RslVar, S, StrT, StrV, Struct, SumDecl, TuplePattern, TupleT, TupleV, UnitT, UnitV, Variable}


class SimplePatternTest extends munit.FunSuite {
  test("eval1-eval") {
    val expr = Letrec(
      List(
        (RslVar("a"), Components(List(I(1), S("str")), 2))
      ),
      Variable("a")
    )

    assertEquals(Resil().eval(expr), TupleV(List(IntV(1), StrV("str")),2))
  }

  test("eval1-typing") {
    val expr = Letrec(
      List(
        (RslVar("a"), Components(List(I(1), S("str")), 2))
      ),
      Variable("a")
    )

    assertEquals(Typing().typecheck(expr), Right(TupleT(List(IntT, StrT))))
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

    assertEquals(Resil().eval(expr), TupleV(List(IntV(97), StrV("bar"), BoolV(false)),3))
  }

  test("eval3-typing") {
    val expr = Letrec(
      List(
        (TuplePattern(List(RslVar("a"), RslVar("b"))),
          Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
      ),
      Variable("b")
    )

    assertEquals(Typing().typecheck(expr), Right(TupleT(List(IntT, StrT, BoolT))))
  }



  test("eval4-eval") {
    val expr = Letrec(
      List(
        (TuplePattern(List(RslVar("a"), RslVar("b"), RslVar("c"))),
          Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
      ),
      Variable("b")
    )

    assertEquals(Resil().eval(expr), ErrV("EvalError: Unapply a tuple of 2 elements to a tuple pattern of 3 items"))
  }

  test("eval4-typing") {
    val expr = Letrec(
      List(
        (TuplePattern(List(RslVar("a"), RslVar("b"), RslVar("c"))),
          Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
      ),
      Variable("b")
    )

    assertEquals(Typing().typecheck(expr), Left("Mismatch tuple sizes"))
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

    assertEquals(Typing().typecheck(expr), Left("Type check error with \u001b[31m(int, string, bool)\u001b[0m and \u001b[31mint\u001b[0m"))
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

    assertEquals(Resil().eval(expr), BoolV(false))
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

    assertEquals(Typing().typecheck(expr), Right(BoolT))
  }
}

class TypedPatternTest extends munit.FunSuite:

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

    assertEquals(Resil().eval(expr1), IntV(3))
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

    assertEquals(Typing().typecheck(expr1), Left("Type check error with \u001b[31mint\u001b[0m and \u001b[31mbool\u001b[0m"))
  }

class CompoundPatternTest extends munit.FunSuite:

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

    assertEquals(Resil().eval(expr), ListV(List(IntV(1), IntV(2), IntV(3), IntV(4))))
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

    assertEquals(Typing().typecheck(expr), Right(ListT(IntT)))
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

    assertEquals(Resil().eval(expr), ListV(List(IntV(1), IntV(2), IntV(3))))
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

    assertEquals(Typing().typecheck(expr), Left("Type check error with \u001b[31mstring\u001b[0m and \u001b[31mint\u001b[0m"))
  }

class ADTPatternTest extends munit.FunSuite:

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

      assertEquals(res1(1), IntV(15))
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

      val res = Typing().typecheck(blocks1)

      assertEquals(Typing().typecheck(blocks1), Right(List(UnitT, IntT)))
    }