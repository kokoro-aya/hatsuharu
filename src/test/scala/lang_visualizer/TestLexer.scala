package lang_visualizer

import moe.irony.resil.lexer.*
import moe.irony.resil.sig.Binary.{ADD, SUB}
import moe.irony.resil.sig.Logical.{EQ, LT}

class TestLexer extends munit.FunSuite:
  test("simple example") {
    val src =
      "{ fn x => fn y => x + y } 1 ((2))"
    val tokens = Tokenizer.tokenize(src)
    val res = List(
      LeftCurlyToken, FnToken, VarToken("x"), DoubleArrowToken, FnToken, VarToken("y"), DoubleArrowToken,
        VarToken("x"), OperToken(ADD), VarToken("y"),
      RightCurlyToken,
        IntToken(1),
        LeftParenToken, LeftParenToken, IntToken(2), RightParenToken, RightParenToken)
    assertEquals(tokens, res)
  }

  test("complex code") {
    val src =
      """
      let rec
        val a = "3"
        val b = if false then true else true == 2 < 3
        val c = 1 + (3 - 4)
        val f = fn x => x + 1
      in
        f ( fst ( (1, 2) ) )
      end
      """
    val tokens = Tokenizer.tokenize(src)
    val res = List(
      LetToken, RecToken, ValToken,
        VarToken("a"), AssignToken, StringToken("3"),
        ValToken, VarToken("b"), AssignToken,
          IfToken, BoolToken(false), ThenToken, BoolToken(true), ElseToken, BoolToken(true),
            OperToken(EQ), IntToken(2), OperToken(LT), IntToken(3),
        ValToken, VarToken("c"), AssignToken,
          IntToken(1), OperToken(ADD), LeftParenToken, IntToken(3), OperToken(SUB), IntToken(4), RightParenToken,
        ValToken, VarToken("f"), AssignToken, FnToken, VarToken("x"), DoubleArrowToken,
          VarToken("x"), OperToken(ADD), IntToken(1),
      InToken,
        VarToken("f"), LeftParenToken, FstToken, LeftParenToken,
          LeftParenToken, IntToken(1), CommaToken, IntToken(2), RightParenToken,
        RightParenToken, RightParenToken,
      EndToken)
    assertEquals(tokens, res)
  }