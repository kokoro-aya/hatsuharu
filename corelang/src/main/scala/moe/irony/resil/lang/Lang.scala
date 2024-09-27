package moe.irony.resil.lang

import moe.irony.resil.sig
import moe.irony.resil.sig.{
  UnionV, TupleV, RecordV, ListV, ArrayV, RefV,
  Data, Components, Struct, ReadonlyList, Array, Ref,
  AUnit, B, Binary, Binop, BoolV, Call, CallDyn, ClosV, Env, ErrV, EvalError, Fst, Func, I, If, IntV, IsAPair, Letrec, Logical, Logop, Pair, PairV, PromV, Rsl, RslExp, RslVal, S, Snd, StrV, UnitV, Variable}


// TODO: refactor this
class ResilEnv[A](val backingField: List[(String, A)] = List()) extends Env[A] {


  override def insert(label: String, value: A): Env[A] =
    ResilEnv[A]((label, value) :: this.backingField)

  override def update(label: String)(newValue: A): Env[A] =
    ResilEnv[A]((label, newValue) :: this.backingField)

  override def lookup(label: String): Option[A] =
    this.backingField.find(label == _._1).map(_._2)

  override def lookupBy(label: String) (criteria: (A) => Boolean): Option[A] =
    this.backingField.find { it => label == it._1 && criteria(it._2) }.map(_._2)

  override infix def ++(other: Env[A]): Env[A] =
    ResilEnv[A](this.backingField ++ other.backingField)

  override def dumpNames: String =
    "[" + this.backingField.map(_._1).mkString(",") + "]"


  // For testing comparison purpose
  override def toString: String =
    "[" + this.backingField.map((s, e) => s + ": " + e).mkString(", ") + "]"
}


def emptyEnv = ResilEnv[RslExp]()


class Resil extends Rsl {

  def showExp(exp: RslExp): String = exp match
    case Data(header, fields) =>
      header ++ "(" ++ fields.map(showExp).mkString(", ") ++ ")"
    case Components(values, _) =>
      "(" ++ values.map(showExp).mkString(", ") ++ ")"
    case Struct(header, values) =>
      header.getOrElse("") ++ " { " ++ values.map { (k, v) => k ++ ": " ++ showExp(v) }.mkString(", ") ++ " }"
    case ReadonlyList(values) =>
      "[" ++ values.map(showExp).mkString(",") ++ "]"
    case Array(elements) =>
      "Array(" ++ elements.map(showExp).mkString(",") ++ ")"
    case Ref(value) => f"Ref(${showExp(value)})"
    case I(value) => f"Int($value)"
    case B(value) => f"Bool($value)"
    case S(value) => f"Str($value)"
    case Variable(label) => f"Var($label)"
    case Binop(_, _, _) => "Binop"
    case Logop(_, _, _) => "Logop"
    case If(_, _, _) => "if"
    case Func(param, body) => f"func($param, ${showExp(body)})"
    case Call(_, _) => "Call"
    case CallDyn(_, _) => "CallDyn"
    case Letrec(_, _) => "Letrec"
    case Pair(_, _) => "Pair"
    case IsAPair(_) => "IsAPair"
    case Fst(_) => "Fst"
    case Snd(_) => "Snd"
    case AUnit() => "Unit"

  override def show(v: RslVal): String = v match
    case UnionV(header, fields) =>
      header ++ "(" ++ fields.map(show).mkString(", ") ++ ")"
    case TupleV(values,  _) =>
      "(" ++ values.map(show).mkString(", ") ++ ")"
    case RecordV(header, values) =>
      header.getOrElse("") ++ " { " ++ values.map { (k, v) => k ++ ": " ++ show(v) }.mkString(", ") ++ " }"
    case ListV(values) =>
      "[" ++ values.map(show).mkString(",") ++ "]"
    case ArrayV(values, length) =>
      f"Array@$length(${values.map(show).mkString(",")})"
    case RefV(value) =>
      f"ref(${show(value)})"
    case IntV(value) => value.toString
    case BoolV(value) => value.toString
    case StrV(value) => value
    case PairV(first, second) => f"$first, $second"
    case UnitV() => "unit"
    case ClosV(_, _) => "#closure"
    case PromV(_) => "#promise"
    case ErrV(msg) => "Error: " + msg

  override def typ(v: RslVal): String = v match
    case UnionV(header, _) =>
      header
    case TupleV(values,  _) =>
      "(" ++ values.map(typ).mkString(", ") ++ ")"
    case RecordV(header, values) =>
      header.getOrElse("") ++ " { " ++ values.map { (k, v) => k ++ ": " ++ typ(v) }.mkString(", ") ++ " }"
    case ListV(values) =>
      "[" ++ values.map(typ).mkString(",") ++ "]"
    case ArrayV(values, length) =>
      f"Array@$length(${values.map(typ).mkString(",")})"
    case RefV(value) =>
      f"ref(${typ(value)})"
    case IntV(_) => "int"
    case BoolV(_) => "bool"
    case StrV(_) => "str"
    case PairV(first, second) => f"(${typ(first)}, ${typ(second)})"
    case UnitV() => "unit"
    case ClosV(_, _) => "closure"
    case PromV(_) => "promise"
    case ErrV(_) => "error"

  override def evalEnv(env: Env[RslVal])(e: RslExp): RslVal = e match
    case I(value) => IntV(value)
    case B(value) => BoolV(value)
    case S(value) => StrV(value)
    case Variable(label) => env.lookupBy(label) {
          // This injected lookup logic means that we will look for first entry which is *either* a promise that holds a
          // value, *or* a value. An unresolved promise will not be taken into account.
      case PromV(Some(_)) => true
      case PromV(None) => false
      case _ => true
    } match
      case Some(x) => x match
        case PromV(Some(v)) => v
        case PromV(None) => throw EvalError("[1] Reading an unset promise while resolving variable name " + label)
        case _ => x
      case None => throw EvalError("[2] Unresolved variable name " + label)
    case Binop(op, left, right) =>
      (evalEnv(env)(left), evalEnv(env)(right)) match
        case (IntV(i1), IntV(i2)) => op match
          case Binary.ADD => IntV (i1 + i2)
          case Binary.SUB => IntV (i1 - i2)
          case Binary.MULT => IntV (i1 * i2)
          case Binary.DIV => IntV (i1 / i2)
          case Binary.MOD => IntV (i1 % i2)
        case (v1, v2) => throw EvalError ("[3] Binop expects integers. Instead found: " + typ (PairV (v1, v2)))
    case Logop(op, left, right) =>
      (evalEnv(env)(left), evalEnv(env)(right)) match
        case (BoolV(b1), BoolV(b2)) => op match
          case Logical.EQ => BoolV(b1 == b2)
          case Logical.NEQ => BoolV(b1 != b2)
          case _ => throw EvalError ("[4] Logop EQ or NEQ expected for bools.")
        case (IntV(i1), IntV(i2)) => op match
          case Logical.LT => BoolV(i1 < i2)
          case Logical.LE => BoolV(i1 <= i2)
          case _ => throw EvalError ("[5] Logop LT or LE expected for ints.")
        case (v1, v2) => throw EvalError ("[6] Logop expects either ints or bools. Instead found: " + typ (PairV (v1, v2)))
    case If(cond, caseTrue, caseElse) => evalEnv(env)(cond) match
      case BoolV(b) => if b then evalEnv(env)(caseTrue) else evalEnv(env)(caseElse)
      case _ => throw EvalError("[7] If operation applied to non-bool")
    case Func(_, _) => ClosV(env = env, f = e)
    case Call(funexp, actual) =>
      val vFn = evalEnv(env)(funexp)
      val vAct = evalEnv(env)(actual)
      vFn match
        case ClosV(innerEnv, f) => f match
          case Func(funArg, funBody) =>
            val newEnv = env // Refill a new env with given environment and add the reference as new scope
            val currEnv = newEnv.insert(funArg, vAct)
            evalEnv(innerEnv ++ currEnv)(funBody)
          case _ => throw EvalError ("[8] call oper. must have a Func as f, got " + showExp(f))
        case _ => throw EvalError ("[9] call oper. must have first subexpr as Closure, got " + show(vFn))
    case CallDyn(dynname, actual) =>
      val vFLabel = evalEnv(env)(dynname)
      val vAct = evalEnv(env)(actual)
      vFLabel match
        case StrV(name) => env.lookup(name) match
          case Some(ClosV(env, f)) => f match
            case Func(funArg, funBody) =>
              val currEnv = ResilEnv[RslVal]().insert(funArg, vAct)
              evalEnv(env ++ currEnv)(funBody)
            case _ => throw EvalError ("[10] call oper. must have a Func as f, got " + showExp(f))
          case _ => throw EvalError ("[11] dyn call on unresolved name " + name + ", known:" + (env.dumpNames))
        case _ => throw EvalError ("[12] dyn call must be invoked on StrV(..), got " + show(vFLabel))
    case Letrec(exps, body) =>
      val prefilledEnvs = exps.backingField.foldLeft(env) { (acc, sv) => acc.insert(sv._1, PromV(None)) }
      val evaledEnvs = exps.backingField.foldLeft(prefilledEnvs) { (acc, sv) =>
        val exi = evalEnv(acc)(sv._2)
        acc.update(sv._1)(exi)
      }
      evalEnv(evaledEnvs)(body)
    case Pair(first, second) =>
      val v1 = evalEnv(env)(first)
      val v2 = evalEnv(env)(second)
      PairV(v1, v2)
    case IsAPair(value) => evalEnv(env)(value) match
      case PairV(_, _) => BoolV(true)
      case _ => BoolV(false)
    case Fst(value) => evalEnv(env)(value) match
      case PairV(e1, _) => e1
      case _ => throw EvalError("[13] fst operation applied to non-pair")
    case Snd(value) => evalEnv(env)(value) match
      case PairV(_, e2) => e2
      case _ => throw EvalError("[14] snd operation applied to non-pair")
    case AUnit() => UnitV()

  override def eval(e: RslExp): RslVal =
    try
      evalEnv(ResilEnv[RslVal]())(e)
    catch {
      case e: EvalError => ErrV("EvalError: " + e.message)
      case e: Throwable => ErrV(e.getMessage)
    }
}