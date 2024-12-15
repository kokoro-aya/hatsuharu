package moe.irony.resil.basic

import moe.irony.resil.lang.{Resil, Typing, newEnvironment}
import moe.irony.resil.sig.*

class ADTTest extends munit.FunSuite:

  test("block1-eval") {
    val blocks1 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List("side" -> IntT)),
            Ctor("Circle", List("radius" -> IntT)),
            Ctor("Rectangle", List("width" -> BoolT, "height" -> StrT)) // Type not checked yet
          )),
        Data("Square", List(I(16))),
        Data("Rectangle", List(I(7), I(8)))
      )

    val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks1)

    assertEquals(res1(1), UnionV("Square", List(IntV(16))))
    assertEquals(res1(2), UnionV("Rectangle", List(IntV(7), IntV(8))))
  }

  test("block1-typing") {
    val blocks1 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List("side" -> IntT)),
            Ctor("Circle", List("radius" -> IntT)),
            Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
          )),
        Data("Square", List(I(16))),
        Data("Rectangle", List(I(7), I(8)))
      )

    assertEquals(Typing().typecheck(blocks1), Right(
      List(
        UnitT, 
        VariantT("Shape", List(), Ctor("Square", List(("side", IntT)))),
        VariantT("Shape", List(), Ctor("Rectangle", List(("width", IntT), ("height", IntT))))
      )))
  }

  test("block1-wrong-typing") {
    val blocks1 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List("side" -> IntT)),
            Ctor("Circle", List("radius" -> IntT)),
            Ctor("Rectangle", List("width" -> BoolT, "height" -> StrT)) // Type not checked yet
          )),
        Data("Square", List(I(16))),
        Data("Rectangle", List(I(7), I(8)))
      )

    assertEquals(Typing().typecheck(blocks1), Left("Type check error with \u001b[31mint\u001b[0m and \u001b[31mstring\u001b[0m"))
  }

  test("block2-eval") {
    val blocks2 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List(("side", IntT))),
            Ctor("Circle", List(("radius", IntT))),
            Ctor("Rectangle", List(("width", IntT), ("height", IntT)))
          )),
        Data("Rectangle", List(S("str"), I(8))) // Will raise mismatch error, but not typed currently
      )

    val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks2)

    assertEquals(res1(1), UnionV("Rectangle", List(StrV("str"), IntV(8))))

  }

  test("block2-typing") {
    val blocks2 =
      List[RslBlock](
        SumDecl(
          "Shape", List(), List(
            Ctor("Square", List(("side", IntT))),
            Ctor("Circle", List(("radius", IntT))),
            Ctor("Rectangle", List(("width", IntT), ("height", IntT)))
          )),
        Data("Rectangle", List(S("str"), I(8))) // Will raise mismatch error, but not typed currently
      )

    assertEquals(Typing().typecheck(blocks2), Left("Type check error with \u001b[31mstring\u001b[0m and \u001b[31mint\u001b[0m"))
  }