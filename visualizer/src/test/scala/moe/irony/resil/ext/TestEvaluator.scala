package moe.irony.resil.ext

import moe.irony.resil.lang
import moe.irony.resil.lang.{Resil, ResilEnv}
import moe.irony.resil.sig.Logical.LT
import moe.irony.resil.sig.*

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
        ("z", Binop(Binary.ADD, Variable("x"), Variable("y")))
      )),
        Variable("z"))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(3))
  }

  test("simple function call") {
    val expr = Call(Func("x", Binop(Binary.ADD, Variable("x"), I(7))), I(1))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(8))
  }

  test("snd of a pair") {
    val expr = Snd (Pair(I(7), I(2)))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(2))
  }

  test("nested func call") {
    val expr = Call (Func ("x", Binop(Binary.ADD, Variable("x"), I(1))), Call(Func ("x", Binop(Binary.MULT, Variable("x"), I(2))), I(3)))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(7))
  }

  test("currying") {
    val expr = Letrec(
      ResilEnv[RslExp](List(
        ("f", Func("x", Binop(Binary.ADD, Variable("x"), I(1)))),
        ("g", Func("x", Binop(Binary.MULT, Variable("x"), I(2)))),
        ("z", I(3))
      )),
      Call(Variable("f"), Call(Variable("g"), Variable("z"))))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(7))
  }

  test("func as arg") {
    val expr = Letrec (
      ResilEnv[RslExp](List(
        ("double", Func("f", Func("#x", Call(Variable("f"), Call(Variable("f"), Variable("#x")))))),
        ("x", I(4))
      )),
      Call(Call(Variable("double"), Func("x", Binop(Binary.ADD, I(2), Variable("x")))), Variable("x")))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(6))
  }

  test("simple func and letrec env var") {
    val expr = Letrec(
      ResilEnv[RslExp](List(
        ("y", I(3)),
        ("f", Func("x", Binop(Binary.ADD, Variable("y"), Variable("x"))))
      )),
      Call(Variable("f"), I(4)))
    val res = Resil().eval(expr)
    assertEquals(res, IntV(7))
  }

  test("forward references") {
    val expr =
      Letrec(
        ResilEnv(
          List(
            ("g", Func("x", Call(Variable("f"), Variable("x")))),
            ("f", Func("x", I(2))),
            ("h", Func("x", Call(Variable("g"), Variable("x"))))
          )),
        Call(Variable("h"), AUnit())
      )
    val res = Resil().eval(expr)
    assertEquals(res, IntV(2))
  }

  test("stackoverflow in case of self-calls") {
    val expr =
      Letrec(
        ResilEnv(
          List(
            ("f", Func("x", Call(Variable("f"), Variable("x"))))
          )
        ),
        Call(Variable("f"), AUnit())
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
              If(Logop(Logical.LT, Variable("x"), I(10)),
                Call(Variable("f"), Binop(Binary.ADD, Variable("x"), I(1))),
                Variable("x")
            )))
          )
        ),
        Call(Variable("f"), I(1)))

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
                            ("area", Func("_", Binop(Binary.MULT, Variable("width"), Variable("height")))),
                            ("perimeter", Func("_", Binop(Binary.MULT, I(2), Binop(Binary.ADD, Variable("width"), Variable("height"))))),
                            ("timesArea", Func("x", Binop(Binary.MULT, Variable("x"), CallDyn(S("area"), AUnit())))))),
                        CallDyn(Variable("fn"), Variable("args"))
                      ))
                  )))),
            ("rect1", Call(Call(Variable("Rectangle"), I(3)), I(4))),  // var rect1 = new Rectangle(3, 4)
            ("rect2", Call(Call(Variable("Rectangle"), I(4)), I(9)))   // var rect2 = new Rectangle(4, 9)
          )),
        Pair(
          Pair(
            Call(Call(Variable("rect1"), S("area")), AUnit()),         // rect1.area == 12
            Call(Call(Variable("rect2"), S("perimeter")), AUnit())     // rect2.perimeter == 26
          ),
          Call(Call(Variable("rect2"), S("timesArea")), I(4)))         // rect2.timesArea(4) == 144
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
                      List(("getName", Func("_", Variable("name"))))),
                      CallDyn(Variable("getter"), AUnit()))))),
            ("Cat",
              Func("name",
                Func("color",
                  Func("getter",
                    Letrec(
                      ResilEnv(
                        List(
                          ("getName", Func("_", Variable("name"))),
                          ("getColor", Func("_", Variable("color"))),
                        )),
                        CallDyn(Variable("getter"), AUnit())))))),
            ("Animal@name", Func("animal", Call(Variable("animal"), S("getName")))),
            ("Animal@description",
              Func("animal",
                Pair(S("my name is"), Call(Variable("Animal@name"), Variable("animal"))))),
            ("Cat@description", Func("cat", Pair(S("meow "), Call(Variable("cat"), S("getColor"))))),
            ("someAnimal", Call(Variable("Animal"), S("Alex"))),
            ("someCat", Call(Call(Variable("Cat"), S("Bob")), S("blue"))))),
        Pair(
          Pair(
            Call(Variable("Animal@description"), Variable("someCat")),
            Call(Variable("Cat@description"), Variable("someCat"))
          ),
          Pair(
            Call(Variable("Animal@name"), Variable("someAnimal")),
            Call(Variable("Animal@name"), Variable("someCat"))
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