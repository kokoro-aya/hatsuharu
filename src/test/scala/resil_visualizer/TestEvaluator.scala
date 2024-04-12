package resil_visualizer

import moe.irony.resil.lang
import moe.irony.resil.lang.Resil
import moe.irony.resil.lang.ResilEnv
import moe.irony.resil.sig.{AUnit, B, Binary, Binop, BoolV, Call, CallDyn, ClosV, Env, ErrV, EvalError, Fst, Func, I, If, IntV, IsAPair, Letrec, Logical, Logop, Pair, PairV, PromV, Rsl, RslExp, RslVal, S, Snd, StrV, UnitV, Var}
import moe.irony.resil.sig.Logical
import moe.irony.resil.sig.Binary
import moe.irony.resil.sig.Logical.LT

class TestEvaluator extends munit.FunSuite:
  test("simple if expr") {
    val expr = If (Logop (Logical.LE, I (3), I (4)), I (3), I ( 2))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(3))
  }

  test("simple binary(add) expr") {
    val expr = Letrec(
      ResilEnv[RslExp](List(
        ("x", I(1)),
        ("y", I(2)),
        ("z", Binop(Binary.ADD, Var("x"), Var("y")))
      )),
        Var("z"))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(3))
  }

  test("simple function call") {
    val expr = Call(Func("x", Binop(Binary.ADD, Var("x"), I(7))), I(1))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(8))
  }

  test("snd of a pair") {
    val expr = Snd (Pair(I(7), I(2)))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(2))
  }

  test("nested func call") {
    val expr = Call (Func ("x", Binop(Binary.ADD, Var("x"), I(1))), Call(Func ("x", Binop(Binary.MULT, Var("x"), I(2))), I(3)))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(7))
  }

  test("currying") {
    val expr = Letrec(
      ResilEnv[RslExp](List(
        ("f", Func("x", Binop(Binary.ADD, Var("x"), I(1)))),
        ("g", Func("x", Binop(Binary.MULT, Var("x"), I(2)))),
        ("z", I(3))
      )),
      Call(Var("f"), Call(Var("g"), Var("z"))))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(7))
  }

  test("func as arg") {
    val expr = Letrec (
      ResilEnv[RslExp](List(
        ("double", Func("f", Func("#x", Call(Var("f"), Call(Var("f"), Var("#x")))))),
        ("x", I(4))
      )),
      Call(Call(Var("double"), Func("x", Binop(Binary.ADD, I(2), Var("x")))), Var("x")))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(6))
  }

  test("simple func and letrec env var") {
    val expr = Letrec(
      ResilEnv[RslExp](List(
        ("y", I(3)),
        ("f", Func("x", Binop(Binary.ADD, Var("y"), Var("x"))))
      )),
      Call(Var("f"), I(4)))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(7))
  }

  test("forward references") {
    val expr =
      Letrec(
        ResilEnv(
          List(
            ("g", Func("x", Call(Var("f"), Var("x")))),
            ("f", Func("x", I(2))),
            ("h", Func("x", Call(Var("g"), Var("x"))))
          )),
        Call(Var("h"), AUnit())
      )
    val res = Resil().eval(expr)
    assertEquals(res, IntV(2))
  }

  test("stackoverflow in case of self-calls") {
    val expr =
      Letrec(
        ResilEnv(
          List(
            ("f", Func("x", Call(Var("f"), Var("x"))))
          )
        ),
        Call(Var("f"), AUnit())
      )

    val res = Resil().eval(expr)
    assertEquals(res, ErrV("RangeError: Maximum call stack size exceeded"))
  }

  test("self reduct to 10") {
    val expr =
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
        Call(Var("f"), I(1)))

    val res = Resil().eval(expr)
    assertEquals(res, IntV(10))
  }