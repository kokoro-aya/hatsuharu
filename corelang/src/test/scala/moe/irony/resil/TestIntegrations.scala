package moe.irony.resil

import moe.irony.resil.lang.Resil
import moe.irony.resil.lexer.Tokenizer
import moe.irony.resil.parser.Parser
import moe.irony.resil.sig.IntV

class TestIntegrations extends munit.FunSuite:

  test("a") {
    val src =
      "if 3 < 4 then 3 else 2"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(3)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("b") {
    val src =
      """
        let rec
          val x = 1
          val y = 2
          val z = x + y
        in
          z
        end
        """.stripMargin
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(3)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("c") {
    val src =
      "call fn x => x + 7 in 1"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(8)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("d") {
    val src =
      "snd (7, 2)"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(2)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("e") {
    val src =
      "call fn x => x + 1 in call fn x => x * 2 in 3"
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(7)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("e1") {
    val src =
      """
        let rec
          val f = fn x => x + 1
          val g = fn x => x * 2
          val z = 3
        in
          call f in call g in z
        end
        """.stripMargin
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(7)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("f") {
    val src =
      """
        let rec
          val double = fn f => fn #x => call f in call f in #x
          val x = 4
        in
          call call double in fn x => 2 + x in x
        end
        """.stripMargin
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(6)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("g") {
    val src =
      """
      let rec
        val y = 3
        val f = fn x => y + x
      in
        call f in 4
      end
      """.stripMargin
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(7)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("g1") {
    val src =
      """
      let rec
        val x = 3
        val y = 4
        val z = 5
        val f = fn x => fn y => fn z =>
          let rec
            val w = 6
          in
            w + x + y + z
          end
      in
        call call call f in x in y in z
      end
      """.stripMargin
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(18)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }

  test("g3") { // # g () instead of g 1, and f ()
    val src =
      """
      let rec
        val f = fn x => call g in 1
        val g = fn x => call h in 1
        val h = fn x => 2
      in
        call f in 2
      end
      """.stripMargin
    val tokens = Tokenizer.tokenize(src)
    val (expr, rem) = Parser().parseExpr(tokens)
    val evalRes = Resil().eval(expr)
    val res = IntV(2)
    assertEquals(evalRes, res)
    assertEquals(rem, List())
  }