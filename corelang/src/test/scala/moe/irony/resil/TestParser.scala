package moe.irony.resil

import moe.irony.resil.lang.ResilEnv
import moe.irony.resil.lexer.Tokenizer
import moe.irony.resil.parser.Parser
import moe.irony.resil.sig.*

class TestParser extends munit.FunSuite:
  test("example 0") {
    val src =
      "call fn x => fn y => x + y in 1"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res =
      Call(
        Func("x", Func("y",
          Binop(Binary.ADD, Variable("x"), Variable("y")
          ))),
        I(1))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  // 简单四则运算

  test(""" "a" * 3 """) {
    val src =
      """ "a" * 3  """
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Binop(Binary.MULT, S("a"), I(3))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("2 + 3") {
    val src =
      "2 + 3"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Binop(Binary.ADD, I(2), I(3))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("3 - 4 * 7") {
    val src =
      "3 - 4 * 7"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Binop(Binary.SUB, I(3), Binop(Binary.MULT, I(4), I(7)))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("a + b * (d - c)") {
    val src =
      "a + b * (d - c)"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Binop(Binary.ADD, Variable("a"), Binop(Binary.MULT, Variable("b"), Binop(Binary.SUB, Variable("d"), Variable("c"))))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("1 + 2 < f == false") {
    val src =
      "1 + 2 < f == false"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Logop(Logical.EQ, Logop(Logical.LT, Binop(Binary.ADD, I(1), I(2)), Variable("f")), B(false))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("((abc))") {
    val src =
      "((abc))"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Variable("abc")
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  // Tuples

  test("(1, 2)") {
    val src =
      "(1, 2)"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Pair(I(1), I(2))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("(a, b, c)") {
    val src =
      "(a, b, c)"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Pair(Variable("a"), Pair(Variable("b"), Variable("c")))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("(e, (f, g), 125)") {
    val src =
      "(e, (f, g), 125)"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Pair(Variable("e"), Pair(Pair(Variable("f"), Variable("g")), I(125)))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("((1 + 1, 2) == (a, b), false)") {
    val src =
      "((1 + 1, 2) == (a, b), false)"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = Pair(Logop(Logical.EQ, Pair(Binop(Binary.ADD, I(1), I(1)), I(2)), Pair(Variable("a"), Variable("b"))), B(false))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  // 函数声明和调用

  test("fn x => x + 1") {
    val src =
      "fn x => x + 1"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res =
      Func("x", Binop(Binary.ADD, Variable("x"), I(1)))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("fn x => fn y => x + y") {
    val src =
      "fn x => fn y => x + y"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res =
      Func("x", Func("y", Binop(Binary.ADD, Variable("x"), Variable("y"))))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  // Let rec

  test("""let rec val a = 3
          val b = false
          val c = "abc" in
        a + b + c
      end""") {
    val src =
      """
        let rec val a = 3
            val b = false
            val c = "abc" in
          a + b + c
        end
        """
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res =
      Letrec(
        ResilEnv[RslExp](List(
          ("a", I(3)),
          ("b", B(false)),
          ("c", S("abc"))
        )),
        body = Binop(Binary.ADD, Variable("a"), Binop(Binary.ADD, Variable("b"), Variable("c")))
      )
    expr match
      case Letrec(env, body) =>
        assertEquals(env.toString, res.env.toString)
        assertEquals(body, res.body)
      case _ => assertEquals(true, false)
    assertEquals(rem, List())
  }

  test("""let rec val foo = 1 in
        foo + bar
      end""") {
    val src =
      """
        let rec val foo = 1 in
          foo + bar
        end
        """
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res =
      Letrec(
        ResilEnv[RslExp](List(
          ("foo", I(1))
        )),
        Binop(Binary.ADD, Variable("foo"), Variable("bar"))
      )
    expr match
      case Letrec(env, body) =>
        assertEquals(env.toString, res.env.toString)
        assertEquals(body, res.body)
      case _ => assertEquals(true, false)
        assertEquals(rem, List())
  }

  // Call & dyn

  test("call fn x => x + 1 in 1") {
    val src =
      "call fn x => x + 1 in 1"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res =
      Call(Func("x", Binop(Binary.ADD, Variable("x"), I(1))), I(1))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }

  test("""call dyn "function" in 2""") {
    val src =
      """call dyn "function" in 2"""
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val res = CallDyn(S("function"), I(2))
    assertEquals(expr, res)
    assertEquals(rem, List())
  }