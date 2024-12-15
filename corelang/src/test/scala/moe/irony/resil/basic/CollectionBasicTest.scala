package moe.irony.resil.basic

import moe.irony.resil.lang.Typing
import moe.irony.resil.sig.{AList, Call, Components, Func, I, IntT, Letrec, ListPattern, ListT, Match, RslVar, Variable}

class CollectionBasicTest extends munit.FunSuite:

  test("TODO-head-typing") {
    val colFuns = Letrec(
      List(
        (RslVar("head"), Func("list", Match(Variable("list"), List(
          (ListPattern(List()), Variable("list")),
          (ListPattern(List(RslVar("x"), RslVar("xs"))), Variable("x"))
        )))),
        (RslVar("tail"), Func("list", Match(Variable("list"), List(
          (ListPattern(List()), Variable("list")),
          (ListPattern(List(RslVar("x"), RslVar("xs"))), Variable("xs"))
        ))))
      ),
      Call(Variable("head"), AList(List(I(3), I(4), I(5), I(6), I(7))))
    )

//    assertEquals(Typing().typecheck(colFuns), Right(IntT))
  }

  test("tail-typing") {
    val colFuns = Letrec(
      List(
        (RslVar("head"), Func("list", Match(Variable("list"), List(
          (ListPattern(List()), Variable("list")),
          (ListPattern(List(RslVar("x"), RslVar("xs"))), Variable("x"))
        )))),
        (RslVar("tail"), Func("list", Match(Variable("list"), List(
          (ListPattern(List()), Variable("list")),
          (ListPattern(List(RslVar("x"), RslVar("xs"))), Variable("xs"))
        ))))
      ),
      Call(Variable("tail"), AList(List(I(3), I(4), I(5), I(6), I(7))))
    )

    assertEquals(Typing().typecheck(colFuns), Right(ListT(IntT)))
  }


   /*
      val map = \f => \ys => ys match
        case [] => []
        case x :: xs => f x :: map xs
   */