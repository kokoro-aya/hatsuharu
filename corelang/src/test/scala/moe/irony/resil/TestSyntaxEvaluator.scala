package moe.irony.resil

import scala.collection.mutable
import moe.irony.resil.lang.{Resil, ResilEnv}
import moe.irony.resil.sig.{I, ListV, *}


class TestSyntaxEvaluator extends munit.FunSuite:
  test("simple list") {
    val expr = ReadonlyList(List(I(1), I(2), I(3)))
    val res = Resil().eval(expr)
    assertEquals(res, ListV(List(IntV(1), IntV(2), IntV(3))))
  }

  test("simple tuple") {
    val expr = Components(List(I(2), B(false), S("foo")), 3)
    val res = Resil().eval(expr)
    assertEquals(res, TupleV(List(IntV(2), BoolV(false), StrV("foo")), 3))
  }

  test("simple record") {
    val expr = Struct(None, Map(
      "a" -> I(3), "b" -> B(false), "c" -> Components(List(I(1), I(2)), 2)))
    val res = Resil().eval(expr)
    assertEquals(res, RecordV(None, Map(
      "a" -> IntV(3), "b" -> BoolV(false), "c" -> TupleV(List(IntV(1), IntV(2)), 2))))
  }

  test("simple array") {
    val expr = Array(mutable.ArraySeq(I(3), I(4), I(5)))
    val res = Resil().eval(expr)
    assertEquals(res, ArrayV(mutable.ArraySeq(IntV(3), IntV(4), IntV(5)), 3))
  }

  test("simple ref") {
    val expr = Ref(I(4))
    val res = Resil().eval(expr)
    assertEquals(res, RefV(IntV(4)))
  }

  test("build tuple with letrec") {
    val expr = Letrec(
      ResilEnv[RslExp](
        List(
          ("a", I(3)),
          ("b", I(4))
        )),
      Letrec(
        ResilEnv[RslExp](List(
          ("c", Components(List(S("foo"), S("bar")), 2))
        )),
        Components(List(Variable("a"), Variable("b"), Variable("c")), 3)
      ))
      val res = Resil().eval(expr)
      assertEquals(res, TupleV(List(IntV(3), IntV(4), TupleV(List(StrV("foo"), StrV("bar")), 2)), 3))
  }

  test("update array value") {
    val expr = Letrec(
      ResilEnv[RslExp](
        List(
          ("a", Array(mutable.ArraySeq(I(3), I(4), I(5)))),
          ("_", Update(Subscript(S("a"), I(2)), I(6)))
        )),
      Variable("a"))
    val res = Resil().eval(expr)
    assertEquals(res, ArrayV(mutable.ArraySeq(IntV(3), IntV(4), IntV(6)), 3))
  }

  test("update ref value") {
    val expr = Letrec(
      ResilEnv[RslExp](
        List(
          ("a", Ref(I(6))),
          ("_", Update(S("a"), I(3)))
        )),
      Variable("a"))
    val res = Resil().eval(expr)
    assertEquals(res, RefV(IntV(3)))
  }
