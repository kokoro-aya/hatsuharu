package moe.irony.resil.datatypes

import moe.irony.resil.lang.{Resil, ResilEnv}
import moe.irony.resil.sig.*

import scala.collection.mutable

class EvaluatorTest extends munit.FunSuite:
  test("simple list") {
    val expr = AList(List(I(1), I(2), I(3)))
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

  test("head and tail of list") {
    val expr = Letrec(
      ResilEnv[RslExp](
        List(
          ("list", AList(List(I(3), I(6), I(11), I(17))))
        )),
      Pair(Head(Variable("list")), Tail(Variable("list"))))
    val res = Resil().eval(expr)
    assertEquals(res, PairV(IntV(3), IntV(17)))
  }

  test("size of list and its middle") {
    val expr = Letrec(
      ResilEnv[RslExp](
        List(
          ("list", AList(List(S("abc"), S("ccc"), S("def"), S("fff"), S("lmn"))))
        )),
      Pair(Size(Variable("list")), Nth(Variable("list"), I(2))))
    val res = Resil().eval(expr)
    assertEquals(res, PairV(IntV(5), StrV("def")))
  }

  test("nth part of tuple") {
    val expr = Letrec(
      ResilEnv[RslExp](
        List(
          ("list", Components(List(S("Hello"), I(12), B(false), S("world")), 4))
        )),
      Pair(NthComponent(Variable("list"), I(2)), NthComponent(Variable("list"), I(1))))
    val res = Resil().eval(expr)
    assertEquals(res, PairV(BoolV(false), IntV(12)))

  }

  // complex test e.g. with function calls