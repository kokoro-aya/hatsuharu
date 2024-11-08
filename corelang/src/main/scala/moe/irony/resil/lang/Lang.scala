package moe.irony.resil.lang

import moe.irony.resil.sig
import moe.irony.resil.sig.{AList, AUnit, Array, ArrayV, B, Binary, Binop, BoolV, Call, CallDyn, ClosV, Components, CompoundSubscript, Data, DataDecl, DataT, Env, Environment, ErrV, EvalError, Fst, Func, Head, I, If, IntV, IsAPair, IsEmpty, Letrec, ListV, Logical, Logop, NamedSubscript, Nth, NthComponent, NumberSubscript, Pair, PairV, PromV, RecordV, Ref, RefV, Rsl, RslBlock, RslDecl, RslExp, RslProgram, RslSubscript, RslType, RslVal, S, Size, Snd, StrV, Struct, Subscript, Tail, TupleV, UnionV, UnitV, Update, Variable}


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


def emptyEnv: Env[RslExp] = ResilEnv[RslExp]()


class Resil extends Rsl {

  def showExp(exp: RslExp): String = exp match
    case Data(header, fields) =>
      header ++ "(" ++ fields.map(showExp).mkString(", ") ++ ")"
    case Components(values, _) =>
      "(" ++ values.map(showExp).mkString(", ") ++ ")"
    case Struct(header, values) =>
      header.getOrElse("") ++ " { " ++ values.map { (k, v) => k ++ ": " ++ showExp(v) }.mkString(", ") ++ " }"
    case AList(values) =>
      "[" ++ values.map(showExp).mkString(", ") ++ "]"
    case Array(elements) =>
      "Array( " ++ elements.map(showExp).mkString(", ") ++ " )"
    case Ref(value) => s"Ref(${showExp(value)})"
    case Update(assignee, assigned) => s"${showExp(assignee)} := ${showExp(assigned)}"
    case Subscript(value, subscript) => s"${showExp(value)}[${showExp(subscript)}]"
    case I(value) => s"$value"
    case B(value) => s"$value"
    case S(value) => s"\"${value}\""
    case Variable(label) => f"$label"
    case Binop(op, left, right) => showExp(left) ++ " " ++  op.show() ++ " " ++   showExp(right)
    case Logop(op, left, right) => showExp(left) ++ " " ++  op.show() ++ " " ++   showExp(right)
    case If(_, _, _) => "if"
    case Func(param, body) => f"\\$param => ${showExp(body)}"
    case Call(f, b) => s"(${showExp(f)} ${showExp(b)})"
    case CallDyn(_, _) => "CallDyn"
    case Letrec(ctx, e) =>
      "let\n"
        ++ ctx.backingField.map((s, ex) => s"  val $s = ${showExp(ex)}").mkString("\n")
        ++ "\n"
        ++ "in\n  "
        ++ showExp(e)
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


  private def buildSubscript(e: RslExp): RslSubscript = e match
    case S(value) => NamedSubscript(value)
    case I(value) => NumberSubscript(value)
    case Subscript(value, subscript) => CompoundSubscript(buildSubscript(value), buildSubscript(subscript))
    case _ => throw EvalError("Currently only (str, int) or nested patterns are allowed")

  override def evalEnv(env: Env[RslVal])(e: RslExp): RslVal = e match
    case Data(header, fields) =>
      throw EvalError("Custom ADT not supported yet")
    case Struct(header, values) =>
      RecordV(header, values = values.map { (k, v) => (k, evalEnv(env)(v)) })
    case Array(elements) =>
      ArrayV(elements.map { v => evalEnv(env)(v) }, elements.size)
    case Ref(value) =>
      RefV(evalEnv(env)(value))
    case Update(assignee, assigned) =>
      buildSubscript(assignee) match
        case NamedSubscript(label) =>
          env.lookup(label) match
            case Some(v: RefV) =>
              v.value = evalEnv(env)(assigned)
              UnitV()
            case Some(v) =>
              throw EvalError("Attempt to apply update on value which is not a ref " + typ (v))
            case None =>
              throw EvalError("Attempt to apply update on non existing ref variable " + label)
        case CompoundSubscript(NamedSubscript(label), NumberSubscript(index)) =>
          env.lookup(label) match
            case Some(v: ArrayV) =>
              if index >= 0 && index < v.length then
                v.values(index) = evalEnv(env)(assigned)
                UnitV()
              else
                throw EvalError(s"Index=$index out of bound for updating array")
            case Some(v) =>
              throw EvalError("Attempt to apply update on value which is not an array " + typ (v))
            case None =>
              throw EvalError("Attempt to apply update on non existing array variable " + label)
        case _ => throw EvalError("Complex subscript patterns not supported yet, only <name> and <name>[<index>] allowed")
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
    case Components(values, arity) =>
      TupleV(values.map { v => evalEnv(env)(v)}, arity)
    case NthComponent(values, pos) => (evalEnv(env)(values), evalEnv(env)(pos)) match
      case (TupleV(values, _), IntV(i)) =>
        values(i)
      case _ => throw EvalError("nth-component operation applied to non-tuple")
    case AList(values) =>
      ListV(values.map { v => evalEnv(env)(v) })
    case Head(list) => evalEnv(env)(list) match
      case ListV(values) =>
        if values.isEmpty then throw EvalError("List out of bound: position 0 of 0")
        else values.head
      case _ => throw EvalError("Unsupported head operation on unknown type")
    case Tail(list) => evalEnv(env)(list) match
      case ListV(values) =>
        if values.isEmpty then throw EvalError("List out of bound: position -1 of 0")
        else values.last
      case _ => throw EvalError("Unsupported tail operation on unknown type")
    case Nth(coll, idx) => (evalEnv(env)(coll), evalEnv(env)(idx)) match
      case (ListV(values), IntV(i)) =>
        if i < 0 || i >= values.size then throw EvalError(s"List out of bound: position $i of ${values.size}")
        else values(i)
      case _ => throw EvalError("Unsupported nth operation on unknown type")
    case Size(list) => evalEnv(env)(list) match
      case ListV(values) => IntV(values.size)
      case _ => throw EvalError("Unsupported size operation on unknown type")
    case IsEmpty(list) => evalEnv(env)(list) match
      case ListV(values) => BoolV(values.isEmpty)
      case _ => throw EvalError("Unsupported isEmpty operation on unknown type")
    case AUnit() => UnitV()

  override def evalEnv(env: Environment)(b: RslBlock): RslVal = b match
    case decl: RslDecl =>
      evalDecl(env)(decl)
      UnitV()
    case exp: RslExp => evalEnv(env.variables)(exp)

  override def evalDecl(env: Environment)(d: RslDecl): Environment = d match
    case DataDecl(name, ctors) =>
      env.lookupType(name) match
        case Some(_) => throw EvalError("Redeclaration of type " ++ name)
        case None =>
          val newType = DataT(name, ctors.map(c => (c.name, c)).toMap)
          env.insert(name, newType)


  override def eval(e: RslExp): RslVal =
    try
      evalEnv(ResilEnv[RslVal]())(e)
    catch {
      case e: EvalError => ErrV("EvalError: " + e.message)
      case e: Throwable => ErrV(e.getMessage)
    }

  override def evalProgram(program: RslProgram): List[RslVal] =
    try {
      evalBlocks(Environment(ResilEnv[RslType](), ResilEnv[RslVal]()))(program.blocks)._2
    } catch {
        case e: EvalError => List(ErrV("EvalError: " + e.message))
        case e: Throwable => List(ErrV(e.getMessage))
      }

}