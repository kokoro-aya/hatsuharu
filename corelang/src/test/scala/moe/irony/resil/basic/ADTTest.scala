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
    
    res1.foreach { it =>
      assertEquals(it, IntV(1))
    }
    
  }

  test("block1-typing") {
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

    assertEquals(Typing().typecheck(blocks1), Right(List(IntT)))
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

    res1.foreach { it =>
      assertEquals(it, IntV(1))
    }

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

    assertEquals(Typing().typecheck(blocks2), Right(List(IntT)))
  }