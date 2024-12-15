package moe.irony.resil.basic

import moe.irony.resil.lang.Typing
import moe.irony.resil.sig.{Call, Components, Func, I, IntT, Letrec, RslVar, Variable}

class LetPolyTest extends munit.FunSuite:

  test("let-poly-typing") {

    // let f = fun x -> (x, x) in
    // let g = f 1 in
    // let h = f "hello"
    val func = Letrec(
      List(
        (RslVar("f"), Func("x", Components(List(Variable("x"), Variable("x")), 2)))
      ),
      Letrec(
        List(
          (RslVar("g"), Call(Variable("f"), I(1)))
        ),
        Variable("f")
      )
    )

    assertEquals(Typing().typecheck(func), Right(IntT))
  }

