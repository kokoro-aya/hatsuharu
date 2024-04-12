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

  test("simple oop") {
    val expr =
      Letrec(
        ResilEnv(
          List(
            ("Rectangle",
              Func("width",
                Func("height",
                  Func("fn",
                    Func("args",
                      Letrec(
                        ResilEnv(
                          List(
                            ("area", Func("_", Binop(Binary.MULT, Var("width"), Var("height")))),
                            ("perimeter", Func("_", Binop(Binary.MULT, I(2), Binop(Binary.ADD, Var("width"), Var("height"))))),
                            ("timesArea", Func("x", Binop(Binary.MULT, Var("x"), CallDyn(S("area"), AUnit())))))),
                        CallDyn(Var("fn"), Var("args"))
                      ))
                  )))),
            ("rect1", Call(Call(Var("Rectangle"), I(3)), I(4))),  // var rect1 = new Rectangle(3, 4)
            ("rect2", Call(Call(Var("Rectangle"), I(4)), I(9)))   // var rect2 = new Rectangle(4, 9)
          )),
        Pair(
          Pair(
            Call(Call(Var("rect1"), S("area")), AUnit()),         // rect1.area == 12
            Call(Call(Var("rect2"), S("perimeter")), AUnit())     // rect2.perimeter == 26
          ),
          Call(Call(Var("rect2"), S("timesArea")), I(4)))         // rect2.timesArea(4) == 144
        )
    val res = Resil().eval(expr)
    assertEquals(res, PairV(PairV(IntV(12), IntV(26)), IntV(144)))
  }

  test("more oop") {
    val expr = 
      Letrec(
        ResilEnv(
          List(
            ("Animal",
              Func("name",
                Func("getter",
                  Letrec(
                    ResilEnv(
                      List(("getName", Func("_", Var("name"))))),
                      CallDyn(Var("getter"), AUnit()))))),
            ("Cat",
              Func("name",
                Func("color",
                  Func("getter",
                    Letrec(
                      ResilEnv(
                        List(
                          ("getName", Func("_", Var("name"))),
                          ("getColor", Func("_", Var("color"))),
                        )),
                        CallDyn(Var("getter"), AUnit())))))),
            ("Animal@name", Func("animal", Call(Var("animal"), S("getName")))),
            ("Animal@description",
              Func("animal",
                Pair(S("my name is"), Call(Var("Animal@name"), Var("animal"))))),
            ("Cat@description", Func("cat", Pair(S("meow "), Call(Var("cat"), S("getColor"))))),
            ("someAnimal", Call(Var("Animal"), S("Alex"))),
            ("someCat", Call(Call(Var("Cat"), S("Bob")), S("blue"))))),
        Pair(
          Pair(
            Call(Var("Animal@description"), Var("someCat")),
            Call(Var("Cat@description"), Var("someCat"))
          ),
          Pair(
            Call(Var("Animal@name"), Var("someAnimal")),
            Call(Var("Animal@name"), Var("someCat"))
          )))
    val res = Resil().eval(expr)                    // class Cat extends class Animal
                                                    // var someAnimal = new Animal("Alex")
    val expected = PairV(                           // var someCat = new Cat("Bob", color="blue")
      PairV(                                        //
        PairV(StrV("my name is"), StrV("Bob")),     // someAnimal.description() = "My name is " + "Bob"
        PairV(StrV("meow "), StrV("blue"))          // someCat.description() = "meow " + "blue"
      ),                                            //
      PairV(                                        //
        StrV("Alex"),                               // someAnimal.name() == "Alex"
        StrV("Bob")                                 // someCat.name() == "Bob"
      ))
    assertEquals(res, expected)
  }