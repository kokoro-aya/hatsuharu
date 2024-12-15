package moe.irony.resil.basic

import moe.irony.resil.lang.{Resil, Typing, newEnvironment}
import moe.irony.resil.sig.{Binary, Binop, Components, Ctor, CtorPattern, Data, Func, I, IntT, IntV, Match, RslBlock, RslVar, S, SumDecl, TypeParamT, TypeVarT, Variable, WildcardPattern}

class MatchingTest extends munit.FunSuite:

  test("matching-1-eval") {
    val blocks2 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List("side" -> IntT)),
            Ctor("Circle", List("radius" -> IntT)),
            Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
          )),
        Match(Data("Rectangle", List(I(15), I(7))),
          List(
            (CtorPattern("Square", List(RslVar("largeness"))), Components(List(S("Square I"), Variable("largeness"), I(0)), 2)),
            (CtorPattern("Rectangle", List(RslVar("w"), RslVar("h"))), Components(List(S("Rect I"), Variable("w"), Variable("h")), 3)),
            (WildcardPattern, Components(List(S("Other case"), I(0), I(0)), 3)),
          )
        ),
        Match(Data("Circle", List(I(22))),
          List(
            (CtorPattern("Square", List(RslVar("largeness"))), Components(List(S("Square II"), Variable("largeness")), 2)),
            (CtorPattern("Rectangle", List(RslVar("w"), RslVar("h"))), Components(List(S("Rect II"), Binop(Binary.ADD, Variable("w"), Variable("h"))), 2)),
            (WildcardPattern, Components(List(S("Other case"), I(0)), 2)),
          )
        )
      )

    val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks2)
    assertEquals(res1(2), IntV(1))
  }

  test("matching-1-typing") {
    val blocks2 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List("side" -> IntT)),
            Ctor("Circle", List("radius" -> IntT)),
            Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
          )),
        Match(Data("Rectangle", List(I(15), I(7))),
          List(
            (CtorPattern("Square", List(RslVar("largeness"))), Components(List(S("Square I"), Variable("largeness"), I(0)), 2)),
            (CtorPattern("Rectangle", List(RslVar("w"), RslVar("h"))), Components(List(S("Rect I"), Variable("w"), Variable("h")), 3)),
            (WildcardPattern, Components(List(S("Other case"), I(0), I(0)), 3)),
          )
        ),
        Match(Data("Circle", List(I(22))),
          List(
            (CtorPattern("Square", List(RslVar("largeness"))), Components(List(S("Square II"), Variable("largeness")), 2)),
            (CtorPattern("Rectangle", List(RslVar("w"), RslVar("h"))), Components(List(S("Rect II"), Binop(Binary.ADD, Variable("w"), Variable("h"))), 2)),
            (WildcardPattern, Components(List(S("Other case"), I(0)), 2)),
          )
        )
      )

    assertEquals(Typing().typecheck(blocks2), Right(List(IntT)))
  }

  test("matching-2-eval") {
    val blocks1 =
      List[RslBlock](
        SumDecl(
          "ListA", List(TypeVarT("A")), List(
            Ctor("Nil", List()),
            Ctor("Cons", List("value" -> TypeVarT("A"), "next" -> TypeParamT("ListA", List(TypeVarT("A")))))
          )),
        SumDecl(
          "Option", List(TypeVarT("A")), List(
            Ctor("None", List()),
            Ctor("Some", List("value" -> TypeVarT("A")))
          )),
        Func("x", Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List())))))))),
        Func("x", Data("Cons", List(
          Binop(Binary.ADD, Variable("x"), I(1)), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List()))))))))
      
      )

    val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks1)
    assertEquals(res1(2), IntV(1))
    assertEquals(res1(3), IntV(1))
  }

  test("matching-2-typing") {
    val blocks1 =
      List[RslBlock](
        SumDecl(
          "ListA", List(TypeVarT("A")), List(
            Ctor("Nil", List()),
            Ctor("Cons", List("value" -> TypeVarT("A"), "next" -> TypeParamT("ListA", List(TypeVarT("A")))))
          )),
        SumDecl(
          "Option", List(TypeVarT("A")), List(
            Ctor("None", List()),
            Ctor("Some", List("value" -> TypeVarT("A")))
          )),
        Func("x", Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List())))))))),
        Func("x", Data("Cons", List(
          Binop(Binary.ADD, Variable("x"), I(1)), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List()))))))))

      )

    assertEquals(Typing().typecheck(blocks1), Right(List(IntT)))
  }

  val blocksCol =
    List[RslBlock](
      SumDecl(
        "ListA", List(TypeVarT("A")), List(
          Ctor("Nil", List()),
          Ctor("Cons", List("value" -> TypeVarT("A"), "next" -> TypeParamT("ListA", List(TypeVarT("A")))))
        )),
      SumDecl(
        "Option", List(TypeVarT("A")), List(
          Ctor("None", List()),
          Ctor("Some", List("value" -> TypeVarT("A")))
        )),
      Func("x", Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List())))))))),
      Func("x", Data("Cons", List(
        Binop(Binary.ADD, Variable("x"), I(1)), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List()))))))))
      //        Func("x", Match(Variable("x"), List(
      //          (CtorPattern("Nil", List()), Data("Nil", List())),
      //          (CtorPattern("Cons", List(RslVar("y"), RslVar("ys"))), Data("Cons", List(
      //            Data("Cons", List(Binop(Binary.ADD, Variable("f"), Variable("x"))))
      //          )))
      //          ))
    )