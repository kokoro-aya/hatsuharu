package moe.irony.resil.lexer

import moe.irony.resil.sig.{Binary, Logical, Op}

sealed trait Token
case class IntToken(value: Int) extends Token
case class BoolToken(value: Boolean) extends Token
case class StringToken(value: String) extends Token
case class VarToken(label: String) extends Token
case class OperToken(op: Op) extends Token
case object IfToken extends Token
case object ThenToken extends Token
case object ElseToken extends Token
case object FnToken extends Token
case object AssignToken extends Token
case object LeftParenToken extends Token
case object RightParenToken extends Token
case object LeftCurlyToken extends Token
case object RightCurlyToken extends Token
case object ArrowToken extends Token
case object DoubleArrowToken extends Token
case object ValToken extends Token
case object EndToken extends Token
case object CallToken extends Token
case object DynToken extends Token
case object LetToken extends Token
case object RecToken extends Token
case object InToken extends Token
case object CommaToken extends Token
case object FstToken extends Token
case object SndToken extends Token
case object UnitToken extends Token
case object SemicolonToken extends Token

object Tokenizer:
  def tokenize(src: String): List[Token] = src match
    case "" => List()
    case s if s(0).isWhitespace =>
      val ws = s.takeWhile(_.isWhitespace)
      tokenize(s.drop(ws.length))
    case s if s(0).isDigit =>
      val digits = s.takeWhile(_.isDigit)
      val remains = s.dropWhile(_.isDigit)
      IntToken(digits.toInt) :: tokenize(remains)
    case s"true$xs" => BoolToken(true) :: tokenize(xs)
    case s"false$xs" => BoolToken(false) :: tokenize(xs)
    case s if s(0) == '"' =>
      val text = s.drop(1).takeWhile(_ != '"')
      val rem = s.drop(1).dropWhile(_ != '"').drop(1)
      StringToken(text) :: tokenize(rem)
    case s"fn$xs" => FnToken :: tokenize(xs)
    case s"if$xs" => IfToken :: tokenize(xs)
    case s"then$xs" => ThenToken :: tokenize(xs)
    case s"else$xs" => ElseToken :: tokenize(xs)
    case s"val$xs" => ValToken :: tokenize(xs)
    case s"end$xs" => EndToken :: tokenize(xs)
    case s"dyn$xs" => DynToken :: tokenize(xs)
    case s"call$xs" => CallToken :: tokenize(xs)
    case s"let$xs" => LetToken :: tokenize(xs)
    case s"rec$xs" => RecToken :: tokenize(xs)
    case s"in$xs" => InToken :: tokenize(xs)
    case s"fst$xs" => FstToken :: tokenize(xs)
    case s"snd$xs" => SndToken :: tokenize(xs)
    case s",$xs" => CommaToken :: tokenize(xs)
    case s";$xs" => SemicolonToken :: tokenize(xs)
    case s"()$xs" => UnitToken :: tokenize(xs)
    case s"=>$xs" => DoubleArrowToken :: tokenize(xs)
    case s"->$xs" => ArrowToken :: tokenize(xs)
    case s"($xs" => LeftParenToken :: tokenize(xs)
    case s"{$xs" => LeftCurlyToken :: tokenize(xs)
    case s")$xs" => RightParenToken :: tokenize(xs)
    case s"}$xs" => RightCurlyToken :: tokenize(xs)
    case s"+$xs" => OperToken(Binary.ADD) :: tokenize(xs)
    case s"-$xs" => OperToken(Binary.SUB) :: tokenize(xs)
    case s"*$xs" => OperToken(Binary.MULT) :: tokenize(xs)
    case s"/$xs" => OperToken(Binary.DIV) :: tokenize(xs)
    case s"%$xs" => OperToken(Binary.MOD) :: tokenize(xs)
    case s"==$xs" => OperToken(Logical.EQ) :: tokenize(xs)
    case s"!=$xs" => OperToken(Logical.NEQ) :: tokenize(xs)
    case s"<=$xs" => OperToken(Logical.LE) :: tokenize(xs)
    case s"<$xs" => OperToken(Logical.LT) :: tokenize(xs)
    case s"=$xs" => AssignToken :: tokenize(xs)
    case s if s(0).isLetter || s(0) == '_' || s(0) == '#' =>
      val lettersOrUnderline = s.takeWhile { it => it.isLetter || it.isDigit || it == '_' || it == '#' }
      val remains = s.drop(lettersOrUnderline.length)
      VarToken(lettersOrUnderline) :: tokenize(remains)
    case _ => throw IllegalStateException("Tokenization failed")