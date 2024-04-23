package moe.irony.resil.parser

import moe.irony.resil.lang.ResilEnv
import moe.irony.resil.lang.Resil
import moe.irony.resil.lexer.*
import moe.irony.resil.sig.*

class Parser:
  def parseProgram(tokens: List[Token]): RslExp =
    val (expr, rem) = parseExpr(tokens)
    if rem.isEmpty then expr
    else
      throw IllegalStateException("There are tokens remained")

  def parseExpr(tokens: List[Token]): (RslExp, List[Token]) =
    if tokens.isEmpty then
      throw IllegalStateException("Empty token stream")
    else
      tokens.head match
        case IntToken(i) => parseExprOrPair(tokens)
        case BoolToken(b) => parseExprOrPair(tokens)
        case StringToken(s) => parseExprOrPair(tokens)
        case VarToken(l) => parseExprOrPair(tokens)
        case IfToken => parseIf(tokens)
        case CallToken =>
          if tokens.size > 2 && tokens(1) == DynToken then
            parseCallDyn(tokens)
          else
            parseCall(tokens)
        case FnToken => parseFunc(tokens)
        case LeftParenToken => parseTerm(tokens)
  
        case LetToken =>
          if tokens.size > 2 && tokens(1) == RecToken then
            parseLetRec(tokens)
          else
            throw IllegalStateException("Expected `rec` in let-rec expression")
        //      case FstToken => ???
        //      case SndToken => ???
        //      case UnitToken => ???
        case _ => throw IllegalStateException("Not an expected token in top-level expr parsing")
  
  def parseIf(tokens: List[Token]): (RslExp, List[Token]) =
    val (condExpr, condCont) = parseExpr(tokens.drop(1))
    if condCont.head != ThenToken then
      throw IllegalStateException("Expected `then` in if expression")
    else
      val (thenExpr, thenCont) = parseExpr(condCont.drop(1))
      if thenCont.head != ElseToken then
        throw IllegalStateException("Expected `else` in if expression")
      else
        val (elseExpr, elseCont) = parseExpr(thenCont.drop(1))
        (If(condExpr, thenExpr, elseExpr), elseCont)
  
  def parseFunc(tokens: List[Token]): (RslExp, List[Token]) =
    val (Variable(str), fnCont) = parseIdentifier(tokens.drop(1))
    if fnCont.head != DoubleArrowToken then
      throw IllegalStateException("Expected `=>` in func expression")
    else
      val (fnBodyExpr, fnAfterCont) = parseExpr(fnCont.drop(1))
      (Func(str, fnBodyExpr), fnAfterCont)
  
  def parseCall(tokens: List[Token]): (RslExp, List[Token]) =
    val (callFnExpr, fnCont) = parseExpr(tokens.drop(1))
    if fnCont.head != InToken then
      throw IllegalStateException("Expected `in` in func call expression")
    else
      val (callActualExpr, actualCont) = parseExpr(fnCont.drop(1))
      (Call(callFnExpr, callActualExpr), actualCont)
  
  def parseCallDyn(tokens: List[Token]): (RslExp, List[Token]) =
    val (dynStrExpr, dynCont) = parseLiteral(tokens.drop(2))
    dynStrExpr match
      case S(_) =>
        if dynCont.head != InToken then
          throw IllegalStateException("Expected `in` in dyn call expression")
        else
          val (dynActualExpr, actualCont) = parseExpr(dynCont.drop(1))
          (CallDyn(dynStrExpr, dynActualExpr), actualCont)
      case _ =>
        throw IllegalStateException("Expected string literal in calldyn expression")
  
  def parseLetRec(tokens: List[Token]): (RslExp, List[Token]) =
    val (assigns, remain) = parseAssigns(tokens.drop(2))
    if remain.head != InToken then
      throw IllegalStateException("Expected `in` in let-rec expression")
    val (expr, letCont) = parseExpr(remain.drop(1))
    if letCont.head != EndToken then
      throw IllegalStateException("Expected `end` in let-rec expression")
    else
      (Letrec(assigns, expr), letCont.drop(1))
  
  def parseAssigns(tokens: List[Token]): (Env[RslExp], List[Token]) =
    if tokens.isEmpty || tokens.head != ValToken then
      (ResilEnv(), tokens)
    else
      val (Variable(s), varCont) = parseIdentifier(tokens.drop(1))
      if varCont.head != AssignToken then
        throw IllegalStateException("Expected `=` in assignment")
      else
        val (bodyExpr, bodyCont) = parseExpr(varCont.drop(1))
        val (contEnv, contAfterAll) = parseAssigns(bodyCont)
        (contEnv.insert(s, bodyExpr), contAfterAll)

  /*
   * Expression parsers
   */

  val rslOperators = Map(
    0 -> List(Logical.EQ, Logical.NEQ),
    1 -> List(Logical.LT, Logical.LE),
    2 -> List(Binary.ADD, Binary.SUB),
    3 -> List(Binary.MULT, Binary.DIV, Binary.MOD),
  )

  def parseExprOrPair(tokens: List[Token]): (RslExp, List[Token]) =
    val (factorExp, afterFactor) = parseExprOfLevel(0, tokens)
    if afterFactor.isEmpty then
      (factorExp, afterFactor)
    else
      if afterFactor.head == CommaToken then
        val (subOpt, afterSub) = parsePairSub(afterFactor)
        subOpt match
          case Some(right) => (Pair(factorExp, right), afterSub)
          case None => (factorExp, afterFactor)
  
      else
        val (subOpt, afterSub) = parseExprSubOfLevel(1, afterFactor)
        subOpt match
          case Some(o, right) =>
            o match
              case binary: Binary => (Binop(binary, factorExp, right), afterSub)
              case logical: Logical => (Logop(logical, factorExp, right), afterSub)
          case None => (factorExp, afterFactor)


  def parsePairSub(tokens: List[Token]): (Option[RslExp], List[Token]) =
    if tokens.isEmpty then
      throw IllegalStateException("expect a complement of pair")
    else tokens.head match
      case CommaToken =>
        val (factorExp, after) =  parseExprOrPair(tokens.drop(1))
        if after.isEmpty then
          (Some(factorExp), after)
        else
          val (nextSubOpt, nextAfter) = parsePairSub(after)
          nextSubOpt match
            case Some(rightExp) => (Some(Pair(factorExp, rightExp)), nextAfter)
            case _ => (Some(factorExp), after)
      case _ => (None, tokens)
  
  def parseExprOfLevel(level: Int, tokens: List[Token]): (RslExp, List[Token]) =
    val (factorExp, afterFactor) =
      if level < (rslOperators.size - 1) then parseExprOfLevel(level + 1, tokens) else parseTerm(tokens)
    if afterFactor.isEmpty then
      (factorExp, afterFactor)
    else
      val (subOpt, afterSub) = parseExprSubOfLevel(level, afterFactor)
      subOpt match
        case Some(o, right) =>
          o match
            case binary: Binary => (Binop(binary, factorExp, right), afterSub)
            case logical: Logical => (Logop(logical, factorExp, right), afterSub)
        case None => (factorExp, afterFactor)
  
  
  
  def parseExprSubOfLevel(level: Int, tokens: List[Token]): (Option[(Op, RslExp)], List[Token]) =
    if tokens.isEmpty then
      throw IllegalStateException("expect a complement of expr")
    else tokens.head match
      case OperToken(o: Op) if rslOperators(level).contains(o) =>
        val (factorExp, after) = if level < (rslOperators.size - 1) then parseExprOfLevel(level + 1, tokens.drop(1)) else parseTerm(tokens.drop(1))
        if after.isEmpty then
          (Some(o, factorExp), after)
        else
          val (nextSubOpt, nextAfter) = parseExprSubOfLevel(level, after)
          nextSubOpt match
            case Some(innerOp, rightExp) =>
              innerOp match
                case binary: Binary => (Some(o, Binop(binary, factorExp, rightExp)), nextAfter)
                case logical: Logical => (Some(o, Logop(logical, factorExp, rightExp)), nextAfter)
            case _ => (Some(o, factorExp), after)
      case _ => (None, tokens)
  
  def parseTerm(tokens: List[Token]): (RslExp, List[Token]) =
    if tokens.isEmpty then
      throw IllegalStateException("expect a term (left paren or literal)")
    else tokens.head match
      case LeftParenToken =>
        val (exprExp, afterExpr) = parseExprOrPair(tokens.drop(1))
        if afterExpr.isEmpty then
          throw IllegalStateException("Expect a right parenthesis")
        afterExpr.head match
          case RightParenToken =>
            (exprExp, afterExpr.tail)
          // Handles pair case in term
          case CommaToken =>
            val (subOpt, afterSub) = parsePairSub(afterExpr)
            subOpt match
              case Some(right) => (Pair(exprExp, right), afterSub)
              case None => (exprExp, afterExpr)
          case _ => throw IllegalStateException("Unmatched left parenthesis")
      case _ => parseLiteral(tokens)
  
  /*
      Fragments
   */
  
  
  def parseLiteral(tokens: List[Token]): (RslExp, List[Token]) =
    if tokens.isEmpty then
      throw IllegalStateException("tokens is empty, expect a literal (int, bool, str or var)")
    else
      val lit = tokens.head match
        case IntToken(i) => I(i)
        case BoolToken(b) => B(b)
        case StringToken(s) => S(s)
        case VarToken(l) => Variable(l)
        case _ => throw IllegalStateException("not a literal token")
      (lit, tokens.tail)
  
  def parseIdentifier(tokens: List[Token]): (Variable, List[Token]) =
    if tokens.isEmpty then
      throw IllegalStateException("tokens is empty, expect an identifier (var)")
    else
      val lit = tokens.head match
        case VarToken(l) => Variable(l)
        case _ => throw IllegalStateException("not a variable token")
      (lit, tokens.tail)