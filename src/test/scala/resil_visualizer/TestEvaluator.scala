package resil_visualizer

import moe.irony.resil.lang
import moe.irony.resil.lang.Resil
import moe.irony.resil.lang.ResilEnv
import moe.irony.resil.sig.{AUnit, B, Binary, Binop, BoolV, Call, CallDyn, ClosV, Env, ErrV, EvalError, Fst, Func, I, If, IntV, IsAPair, Letrec, Logical, Logop, Pair, PairV, PromV, Rsl, RslExp, RslVal, S, Snd, StrV, UnitV, Var}
import moe.irony.resil.sig.Logical
import moe.irony.resil.sig.Binary
import moe.irony.resil.sig.Logical.LT

class TestEvaluator extends munit.FunSuite:
  test("a") {
    val expr = Resil().eval ( If (Logop (Logical.LE, I (3), I (4)), I (3), I ( 2)))
    assertEquals(expr, IntV(3))
  }

  test("b") {
    val expr = Resil().eval (Letrec(
      ResilEnv[RslExp](List(
        ("x", I(1)),
        ("y", I(2)),
        ("z", Binop(Binary.ADD, Var("x"), Var("y")))
      )),
        Var("z")))
    assertEquals(expr, IntV(3))
  }

  test("c") {
    val expr = Resil().eval (Call(Func("x", Binop(Binary.ADD, Var("x"), I(7))), I(1)))
    assertEquals(expr, IntV(8))
  }

  test("d") {
    val expr = Resil().eval (Snd (Pair(I(7), I(2))))
    assertEquals(expr, IntV(2))
  }

  test("e") {
    val expr = Resil().eval (Call (Func ("x", Binop(Binary.ADD, Var("x"), I(1))), Call(Func ("x", Binop(Binary.MULT, Var("x"), I(2))), I(3))))
    assertEquals(expr, IntV(7))
  }

  test("e1") {
    val expr = Resil().eval (Letrec(
      ResilEnv[RslExp](List(
        ("f", Func("x", Binop(Binary.ADD, Var("x"), I(1)))),
        ("g", Func("x", Binop(Binary.MULT, Var("x"), I(2)))),
        ("z", I(3))
      )),
      Call(Var("f"), Call(Var("g"), Var("z")))))
    assertEquals(expr, IntV(7))
  }

  test("f") {
    val expr = Resil().eval(Letrec (
      ResilEnv[RslExp](List(
        ("double", Func("f", Func("#x", Call(Var("f"), Call(Var("f"), Var("#x")))))),
        ("x", I(4))
      )),
      Call(Call(Var("double"), Func("x", Binop(Binary.ADD, I(2), Var("x")))), Var("x"))))
    assertEquals(expr, IntV(6))
  }

  test("g") {
    val expr = Resil().eval(Letrec(
      ResilEnv[RslExp](List(
        ("y", I(3)),
        ("f", Func("x", Binop(Binary.ADD, Var("y"), Var("x"))))
      )),
      Call(Var("f"), I(4))))
    assertEquals(expr, IntV(7))
  }

  test("forward references") {
    val expr = Resil().eval(
      Letrec(
        ResilEnv(
          List(
            ("g", Func("x", Call(Var("f"), Var("x")))),
            ("f", Func("x", I(2))),
            ("h", Func("x", Call(Var("g"), Var("x"))))
          )),
        Call(Var("h"), AUnit())
      ))

    assertEquals(expr, IntV(2))
  }

  test("stackoverflow in case of self-calls") {
    val expr = Resil().eval(Letrec(
      ResilEnv(
        List(
          ("f", Func("x", Call(Var("f"), Var("x"))))
        )
      ),
      Call(Var("f"), AUnit())
    ))

    assertEquals(expr, ErrV("RangeError: Maximum call stack size exceeded"))
  }

  test("self reduct to 10") {
    val expr = Resil().eval(
      Letrec(
      ResilEnv(
        List(
          ("f", Func("x",
            If(Logop(Logical.LT, Var("x"), I(10)),
              Call(Var("f"), Binop(Binary.ADD, Var("x"), I(1))),
              Var("x")
          )))
        )
      ),
      Call(Var("f"), I(1))))

    assertEquals(expr, IntV(10))
  }