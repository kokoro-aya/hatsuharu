package moe.irony.resil.basic

import moe.irony.resil.lang.{ResilEnv, TypeError, Typing}
import moe.irony.resil.sig.*
import moe.irony.resil.sig.Binary.*
import moe.irony.resil.sig.Logical.*

def mkEnv(bindings: (String, RslExp)*): ResilEnv[RslExp] = {
  ResilEnv[RslExp](bindings.toList)
}


class SimpleTypingTest extends munit.FunSuite:

  test("exp1") {
    val exp1 = I(5)
    assertEquals(Typing().typecheck(exp1), Right(IntT))
  }

  test("exp2") {
    val exp2 = B(false)
    assertEquals(Typing().typecheck(exp2), Right(BoolT))
  }

  test("exp3") {
    val exp3 = Pair(I(5), I(6))
    assertEquals(Typing().typecheck(exp3), Right(PairT(IntT, IntT)))
  }

  test("exp4") {
    val exp4 = Pair(Pair(S("foo"), I(3)), AUnit())
    assertEquals(Typing().typecheck(exp4), Right(PairT(PairT(StrT, IntT), UnitT)))
  }

  test("bin1") {
    val bin1 = Binop(ADD, I(3), I(4))
    assertEquals(Typing().typecheck(bin1), Right(IntT))
  }

  test("bin2") {
    val bin2 = Logop(LE, I(3), I(4))
    assertEquals(Typing().typecheck(bin2), Right(BoolT))
  }

  test("bin3") {
    val bin3 = Logop(EQ, B(true), B(false))
    assertEquals(Typing().typecheck(bin3), Right(BoolT))
  }

  test("pairfst") {
    val pairfst = Fst(Pair(Pair(S("foo"), I(3)), AUnit()))
    assertEquals(Typing().typecheck(pairfst), Right(PairT(StrT, IntT)))
  }

  test("pairsnd") {
    val pairsnd = Snd(Pair(Pair(AUnit(), I(4)), Pair(Pair(I(9), S("bar")), AUnit())))
    assertEquals(Typing().typecheck(pairsnd), Right(PairT(PairT(IntT, StrT), UnitT)))
  }

  test("if1") {
    val if1 = If(Logop(LE, I(3), I(4)), I(3), I(2))
    assertEquals(Typing().typecheck(if1), Right(IntT))
  }

  test("func1") {
    val func1 = Func("x", I(1))
    assertEquals(Typing().typecheck(func1), Right(FuncT(ParamT("A"), IntT)))
  }

  test("func2") {
    val func2 = Func("x", Binop(ADD, I(3), Variable("x")))
    assertEquals(Typing().typecheck(func2), Right(FuncT(IntT, IntT)))
  }

  test("func3") {
    val func3 = Func("x", Func("y", Binop(ADD, Variable("x"), Variable("y"))))
    assertEquals(Typing().typecheck(func3), Right(FuncT(IntT, FuncT(IntT, IntT))))
  }

  test("fnGen1") {
    val fnGen1 = Func("x", Variable("x"))

    assertEquals(Typing().typecheck(fnGen1), Right(FuncT(ParamT("A"), ParamT("A"))))
  }

  test("letrec1") {
    val letrec1 = Letrec(
      mkEnv(("x", I(3))),
      Variable("x")
    )

    assertEquals(Typing().typecheck(letrec1), Right(IntT))
  }

  test("letrec2") {
    val letrec2 = Letrec(
      mkEnv(
        ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
        ("x", I(3)),
        ("y", I(4))
      ),
      Variable("f")
    )

    assertEquals(Typing().typecheck(letrec2), Right(FuncT(IntT, IntT)))
  }

  test("letrec3") {
    val letrec3 = Letrec(
      mkEnv(
        ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
        ("x", I(3)),
        ("y", I(4))
      ),
      Binop(ADD, Variable("y"), Variable("x"))
    )

    assertEquals(Typing().typecheck(letrec3), Right(IntT))
  }

  test("letrec4") {
    val letrec4 = Letrec(
      mkEnv(
        ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
        ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
        ("z", I(3))
      ),
      Pair(Variable("f"), Variable("g"))
    )

    assertEquals(Typing().typecheck(letrec4), Right(PairT(FuncT(IntT, IntT), FuncT(IntT, IntT))))
  }

  test("letrec5") {
    val letrec5 = Letrec(
      mkEnv(
        ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
        ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
        ("z", I(3))
      ),
      (Call(Variable("f"), Call(Variable("g"), Variable("z"))))
    )

    assertEquals(Typing().typecheck(letrec5), Right(IntT))
  }

  test("call1") {
    val call1 = Call(Func("x", Binop(ADD, Variable("x"), I(1))), Call(Func("x", Binop(MULT, Variable("x"), I(2))), I(3)))

    assertEquals(Typing().typecheck(call1), Right(IntT))
  }

  test("a") {
    val a = If(Logop(LE, I(3), I(4)), I(3), I(2))

    assertEquals(Typing().typecheck(a), Right(IntT))
  }

  test("b") {
    val b = Letrec(
      mkEnv(
        ("x", I(1)),
        ("y", I(2)),
        ("z", Binop(ADD, Variable("x"), Variable("y")))
      ),
      Variable("z")
    )

    assertEquals(Typing().typecheck(b), Right(IntT))
  }

  test("c") {
    val c = Call(Func("x", Binop(ADD, Variable("x"), I(7))), I(1))

    assertEquals(Typing().typecheck(c), Right(IntT))
  }

  test("d") {
    val d = Snd(Pair(I(7), I(2)))

    assertEquals(Typing().typecheck(d), Right(IntT))
  }

  test("e") {
    val e = Call(Func("x", Binop(ADD, Variable("x"), I(1))), Call(Func("x", Binop(MULT, Variable("x"), I(2))), I(3)))

    assertEquals(Typing().typecheck(e), Right(IntT))
  }

  test("e1") {
    val e1 = Letrec(
      mkEnv(
        ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
        ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
        ("z", I(3))
      ),
      Call(Variable("f"), Call(Variable("g"), Variable("z")))
    )

    assertEquals(Typing().typecheck(e1), Right(IntT))
  }

  test("f") {
    val f = Letrec(
      mkEnv(
        ("double", Func("f", Func("#x", Call(Variable("f"), Call(Variable("f"), Variable("#x")))))),
        ("x", I(4))
      ),
      Call(Call(Variable("double"), Func("x", Binop(ADD, I(2), Variable("x")))), Variable("x"))
    )

    assertEquals(Typing().typecheck(f), Right(IntT))
  }


  test("f_") {
    val fx = Func("f", Func("#x", Call(Variable("f"), Variable("#x"))))

    assertEquals(Typing().typecheck(fx), Right(FuncT(FuncT(ParamT("A"), ParamT("B")), FuncT(ParamT("A"), ParamT("B")))))
  }

  test("_f") {
    val fx = Func("#x", Func("f", Call(Variable("f"), Call(Variable("f"), Variable("#x")))))

    assertEquals(Typing().typecheck(fx), Right(FuncT(ParamT("A"), FuncT(FuncT(ParamT("A"), ParamT("A")), ParamT("A")))))
  }


  test("f0") {
    val f0 = Func("f", Func("#x", Call(Variable("f"), Call(Variable("f"), Variable("#x")))))

    assertEquals(Typing().typecheck(f0), Right(FuncT(FuncT(ParamT("A"), ParamT("A")), FuncT(ParamT("A"), ParamT("A")))))
  }

  test("f1") {
    val f1 = Letrec(
      mkEnv(
        ("double", Func("f", Func("#x", Call(Variable("f"), Call(Variable("f"), Variable("#x")))))),
        ("x", I(4)),
        ("y", I(5))
      ),
      Variable("double")
    )

    assertEquals(Typing().typecheck(f1), Right(FuncT(FuncT(ParamT("A"), ParamT("A")), FuncT(ParamT("A"), ParamT("A")))))
  }

  test("fx") {
    val fx = Letrec(
      mkEnv(("x", I(4))),
      Variable("x")
    )

    assertEquals(Typing().typecheck(fx), Right(IntT))
  }

  test("f2") {
    val f2 = Letrec(
      mkEnv(
        ("double", Func("f", Func("#x", Call(Variable("f"), Call(Variable("f"), Variable("#x")))))),
        ("x", I(4))
      ),
      Variable("x")
    )

    assertEquals(Typing().typecheck(f2), Right(IntT))
  }

  test("f3") {
    val f3 = Func("f", Func("#x", Call(Variable("f"), Variable("#x"))))

    assertEquals(Typing().typecheck(f3), Right(FuncT(FuncT(ParamT("A"), ParamT("B")), FuncT(ParamT("A"), ParamT("B")))))
  }

  test("g") {
    val g = Letrec(
      mkEnv(
        ("y", I(3)),
        ("f", Func("x", Binop(ADD, Variable("y"), Variable("x"))))
      ),
      Call(Variable("f"), I(4))
    )

    assertEquals(Typing().typecheck(g), Right(IntT))
  }

  test("g1") {
    val g1 = Letrec(
      mkEnv(
        ("x", I(3)),
        ("y", I(4)),
        ("z", I(5)),
        ("f", Func("x",
          Func("y",
            Func("z",
              Letrec(
                mkEnv(("w", I(6))
                ),
                Binop(ADD, Variable("w"),
                  Binop(ADD, Variable("x"),
                    Binop(ADD, Variable("y"), Variable("z"))))
              ))))
        )
      ),
      Call(Call(Call(Variable("f"), Variable("x")), Variable("y")), Variable("z"))
    )

    assertEquals(Typing().typecheck(g1), Right(IntT))
  }

  // TODO: Forwarding is not supported yet, see how it was implemented in exp
  test("g3") {
    val g3 = Letrec(
      mkEnv(
        ("f", Func("x", Call(Variable("g"), AUnit()))),
        ("g", Func("x", Call(Variable("h"), AUnit()))),
        ("h", Func("x", I(2)))
      ),
      Call(Variable("f"), AUnit())
    )

    assertEquals(Typing().typecheck(g3), Left("Unknown variable g"))
  }
