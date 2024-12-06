package moe.irony.resil.lang

import moe.irony.resil.sig
import moe.irony.resil.sig.{AList, AUnit, Array, ArrayV, B, Binary, Binop, BoolV, Call, CallDyn, ClosV, Components, CompoundSubscript, CtorPattern, Data, DataT, Env, Environment, ErrV, EvalError, Fst, Func, Head, I, If, IntV, IsAPair, IsEmpty, Letrec, ListPattern, ListV, Logical, Logop, NamedSubscript, Nth, NthComponent, NumberSubscript, Pair, PairV, PromV, RecordPattern, RecordV, Ref, RefV, Rsl, RslAssignable, RslBlock, RslDecl, RslExp, RslPattern, RslProgram, RslSubscript, RslType, RslVal, RslVar, S, Size, Snd, StrV, Struct, Subscript, SumDecl, Tail, TuplePattern, TupleV, UnionV, UnitV, Update, Variable, WildcardPattern}


// TODO: refactor this
class ResilEnv[A](val backingField: List[(String, A)] = List()) extends Env[A] {


  override def insert(label: String, value: A): Env[A] =
    ResilEnv[A]((label, value) :: this.backingField)

  override def update(label: String)(newValue: A): Env[A] =
    ResilEnv[A]((label, newValue) :: this.backingField)

  override def lookup(label: String): Option[A] =
    this.backingField.find(label == _._1).map(_._2)

  override def lookupBy(label: String) (criteria: (A) => Boolean): Option[A] =
    this.backingField.find { it => it._1 == label && criteria(it._2) }.map(_._2)

  override def lookupBy(criteria: (A) => Boolean): Option[A] =
    this.backingField.find { it => criteria(it._2) }.map(_._2)


  override infix def ++(other: Env[A]): Env[A] =
    ResilEnv[A](this.backingField ++ other.backingField)

  override def dumpNames: String =
    "[" + this.backingField.map(_._1).mkString(",") + "]"


  // For testing comparison purpose
  override def toString: String =
    "[" + this.backingField.map((s, e) => s + ": " + e).mkString(", ") + "]"
}


def emptyEnv: Env[RslExp] = ResilEnv[RslExp]()

def newEnvironment: Environment =
  Environment(
    ResilEnv[RslType](),
    ResilEnv[RslVal]()
  )


class Resil extends Rsl {
  
  def showSubscript(s: RslSubscript): String = s match
    case NamedSubscript(label) => s".$label"
    case NumberSubscript(value) => s"[$value]"
    case CompoundSubscript(rslVar, subscript) => rslVar.label ++ showSubscript(subscript)

  def showAssignable(a: RslAssignable) = a match
    case p: RslPattern => p match
      case TuplePattern(items) => "(" ++ items.mkString(", ") ++ ")"
      case ListPattern(elements) => elements.mkString(" :: ")
      case RecordPattern(fields) => "{" ++ fields.mkString(", ") ++ "}"
      case CtorPattern(name, fields) => name ++ "(" ++ fields.mkString(", ") ++ ")"
      case WildcardPattern => "_"
    case RslVar(label) => label

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
        ++ ctx.map((s, ex) => s"  val ${showAssignable(s)} = ${showExp(ex)}").mkString("\n")
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
    case Subscript(Variable(label), subscript) => CompoundSubscript(RslVar(label), buildSubscript(subscript))
    case _ => throw EvalError("Currently only subscript based on variable with (int, str) subscripts are allowed")


  def preUnapplyPattern(a: RslAssignable): List[(String, RslVal)] = {
    a match
      case _: RslSubscript => throw EvalError("Unapply pattern not applicable to subscripts")
      case RslVar(label) =>
        List((label, PromV(None)))
      case p: RslPattern => p match
        case WildcardPattern => List()
        case TuplePattern(items) => items flatMap preUnapplyPattern
        case ListPattern(elements) => elements flatMap preUnapplyPattern
        case RecordPattern(fields) => List()
        case CtorPattern(name, fields) => List()
  }
    
  def unapplyPattern (a: RslAssignable) (vl: RslVal): List[(String, RslVal)] = {
    a match
      case _: RslSubscript => throw EvalError("Unapply pattern not applicable to subscripts")
      case RslVar(label) => 
        List((label, vl))
      case p: RslPattern => p match
        case WildcardPattern => List()
        case RecordPattern(fields) => vl match
          case RecordV(header, values) =>
            val fieldNames = fields.map(_.label).toSet
            val actualNames = values.keySet
            if fieldNames == actualNames then
              values.toList
            else
              throw EvalError("Unapply a record with record pattern - mismatch fields")
          case _ => throw EvalError("Unapply a non record value on a record pattern")
        case CtorPattern(name, fields) => vl match
          case UnionV(header, values) =>
            if fields.size == values.size then
              fields.map(_.label) zip values
            else
              throw EvalError("Unapply an union value with constructor pattern - mismatch fields")
          case _ => throw EvalError("Unapply a non record value on a record pattern")
        case TuplePattern(items) => vl match
          case TupleV(values, arity) =>
            if arity == items.size then
              (items zip values).flatMap { (p, v) => unapplyPattern(p)(v) }
            else
              throw EvalError(s"Unapply a tuple of $arity elements to a tuple pattern of ${items.size} items")
          case _ => throw EvalError("Unapply a non tuple value on a tuple pattern")
        case ListPattern(elements) => vl match
          case ArrayV(values, length) =>
            if length == elements.size then
              (elements zip values).flatMap { (p, v) => unapplyPattern(p)(v) }
            else
              throw EvalError(s"Unapply an array of $length elements to a list pattern of ${elements.size} items")
          case ListV(values) =>
            if elements.isEmpty then
              if values.isEmpty then
                return List()
              else
                throw EvalError("Unapply a non empty list to an empty list pattern")
            if elements.size == 1 then
              unapplyPattern(elements.head)(vl)
            else if elements.size <= values.size then
              unapplyPattern(elements.head)(values.head) ++ unapplyPattern(ListPattern(elements.tail))(ListV(values.tail))
            else
              throw EvalError(s"Unapply a list of ${values.size} elements to a list pattern of ${elements.size} items")
          case _ => throw EvalError("Unapply a non list value on a list pattern")
          // TODO: array
  }
  
  def evaluateAndFillLetRecEnv(env: Environment) (exps: List[(RslAssignable, RslExp)]): Environment = {
    val precomputedNewVarsAndAssignees = exps.map(_._1).flatMap(preUnapplyPattern)
    val prefilledEnvs = precomputedNewVarsAndAssignees.foldLeft(env._2) { (acc, sv) => acc.insert(sv._1, sv._2) }
    val evaluatedNewVarsAndAssignees = exps.foldLeft(prefilledEnvs) { (acc, sv) =>
      val exi = evalExp(Environment(env._1, acc)) (sv._2)
      val unapplied = unapplyPattern(sv._1)(exi)
      unapplied.foldLeft(acc) { (accEnv, curr) =>
        accEnv.update(curr._1) (curr._2)
      }
    }
    Environment(env._1, evaluatedNewVarsAndAssignees)
  }

  override def evalExp(env: Environment)(e: RslExp): RslVal = {
    e match
    case Data(label, fields) =>
      env.lookupTypeByCriteria {
        case DataT(_, _) => true
        case _ => false
      } match {
        case Some(DataT(sumName, ctors)) =>
          ctors.get(label) match
            case Some(ctorLabel, ctorFields)  =>
              if ctorFields.size == fields.size then
                // TODO: type check to enforce typing of each field
                UnionV(ctorLabel, fields.map(evalExp(env)(_)))
              else
                throw EvalError(s"The type constructor $sumName::$label has ${ctorFields.size} fields, required: ${fields.size}")
            case None =>
              throw EvalError(s"Cannot find registered union type with constructor $label")
        case Some(_) => throw EvalError(s"The type variant $label is not a custom ADT type")
        case None => throw EvalError(s"Type variant $label not found")
      }
    case Struct(header, values) =>
      RecordV(header, values = values.map { (k, v) => (k, evalExp(env)(v)) })
    case Array(elements) =>
      ArrayV(elements.map { v => evalExp(env)(v) }, elements.size)
    case Ref(value) =>
      RefV(evalExp(env)(value))
    case Update(assignee, assigned) =>
      buildSubscript(assignee) match
        case NamedSubscript(label) =>
          env._2.lookup(label) match
            case Some(v: RefV) =>
              v.value = evalExp(env)(assigned)
              UnitV()
            case Some(v) =>
              throw EvalError("Attempt to apply update on value which is not a ref " + typ (v))
            case None =>
              throw EvalError("Attempt to apply update on non existing ref variable " + label)
        case CompoundSubscript(RslVar(label), NumberSubscript(index)) =>
          env._2.lookup(label) match
            case Some(v: ArrayV) =>
              if index >= 0 && index < v.length then
                v.values(index) = evalExp(env)(assigned)
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
    case Variable(label) => env._2.lookupBy(label) {
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
      (evalExp(env)(left), evalExp(env)(right)) match
        case (IntV(i1), IntV(i2)) => op match
          case Binary.ADD => IntV (i1 + i2)
          case Binary.SUB => IntV (i1 - i2)
          case Binary.MULT => IntV (i1 * i2)
          case Binary.DIV => IntV (i1 / i2)
          case Binary.MOD => IntV (i1 % i2)
        case (v1, v2) => throw EvalError ("[3] Binop expects integers. Instead found: " + typ (PairV (v1, v2)))
    case Logop(op, left, right) =>
      (evalExp(env)(left), evalExp(env)(right)) match
        case (BoolV(b1), BoolV(b2)) => op match
          case Logical.EQ => BoolV(b1 == b2)
          case Logical.NEQ => BoolV(b1 != b2)
          case _ => throw EvalError ("[4] Logop EQ or NEQ expected for bools.")
        case (IntV(i1), IntV(i2)) => op match
          case Logical.LT => BoolV(i1 < i2)
          case Logical.LE => BoolV(i1 <= i2)
          case _ => throw EvalError ("[5] Logop LT or LE expected for ints.")
        case (v1, v2) => throw EvalError ("[6] Logop expects either ints or bools. Instead found: " + typ (PairV (v1, v2)))
    case If(cond, caseTrue, caseElse) => evalExp(env)(cond) match
      case BoolV(b) => if b then evalExp(env)(caseTrue) else evalExp(env)(caseElse)
      case _ => throw EvalError("[7] If operation applied to non-bool")
    case Func(_, _) => ClosV(env = env._2, f = e)
    case Call(funexp, actual) =>
      val vFn = evalExp(env)(funexp)
      val vAct = evalExp(env)(actual)
      vFn match
        case ClosV(innerEnv, f) => f match
          case Func(funArg, funBody) =>
            val newEnv = env._2 // Refill a new env with given environment and add the reference as new scope
            val currEnv = newEnv.insert(funArg, vAct)
            evalExp(Environment(env._1, innerEnv ++ currEnv))(funBody)
          case _ => throw EvalError ("[8] call oper. must have a Func as f, got " + showExp(f))
        case _ => throw EvalError ("[9] call oper. must have first subexpr as Closure, got " + show(vFn))
    case CallDyn(dynname, actual) =>
      val vFLabel = evalExp(env)(dynname)
      val vAct = evalExp(env)(actual)
      vFLabel match
        case StrV(name) => env._2.lookup(name) match
          case Some(ClosV(cenv, f)) => f match
            case Func(funArg, funBody) =>
              val currEnv = ResilEnv[RslVal]().insert(funArg, vAct)
              evalExp(Environment(env._1, cenv ++ currEnv))(funBody)
            case _ => throw EvalError ("[10] call oper. must have a Func as f, got " + showExp(f))
          case _ => throw EvalError ("[11] dyn call on unresolved name " + name + ", known:" + (env._2.dumpNames))
        case _ => throw EvalError ("[12] dyn call must be invoked on StrV(..), got " + show(vFLabel))
    case Letrec(exps, body) =>
      val scopedRecEnv = evaluateAndFillLetRecEnv(env)(exps)
      evalExp(scopedRecEnv)(body)
    case Pair(first, second) =>
      val v1 = evalExp(env)(first)
      val v2 = evalExp(env)(second)
      PairV(v1, v2)
    case IsAPair(value) => evalExp(env)(value) match
      case PairV(_, _) => BoolV(true)
      case _ => BoolV(false)
    case Fst(value) => evalExp(env)(value) match
      case PairV(e1, _) => e1
      case _ => throw EvalError("[13] fst operation applied to non-pair")
    case Snd(value) => evalExp(env)(value) match
      case PairV(_, e2) => e2
      case _ => throw EvalError("[14] snd operation applied to non-pair")
    case Components(values, arity) =>
      TupleV(values.map { v => evalExp(env)(v)}, arity)
    case NthComponent(values, pos) => (evalExp(env)(values), evalExp(env)(pos)) match
      case (TupleV(values, _), IntV(i)) =>
        values(i)
      case _ => throw EvalError("nth-component operation applied to non-tuple")
    case AList(values) =>
      ListV(values.map { v => evalExp(env)(v) })
    case Head(list) => evalExp(env)(list) match
      case ListV(values) =>
        if values.isEmpty then throw EvalError("List out of bound: position 0 of 0")
        else values.head
      case _ => throw EvalError("Unsupported head operation on unknown type")
    case Tail(list) => evalExp(env)(list) match
      case ListV(values) =>
        if values.isEmpty then throw EvalError("List out of bound: position -1 of 0")
        else values.last
      case _ => throw EvalError("Unsupported tail operation on unknown type")
    case Nth(coll, idx) => (evalExp(env)(coll), evalExp(env)(idx)) match
      case (ListV(values), IntV(i)) =>
        if i < 0 || i >= values.size then throw EvalError(s"List out of bound: position $i of ${values.size}")
        else values(i)
      case _ => throw EvalError("Unsupported nth operation on unknown type")
    case Size(list) => evalExp(env)(list) match
      case ListV(values) => IntV(values.size)
      case _ => throw EvalError("Unsupported size operation on unknown type")
    case IsEmpty(list) => evalExp(env)(list) match
      case ListV(values) => BoolV(values.isEmpty)
      case _ => throw EvalError("Unsupported isEmpty operation on unknown type")
    case AUnit() => UnitV()
  }

  override def evalBlock(env: Environment)(b: RslBlock): (Environment, RslVal) = b match
    case decl: RslDecl =>
      (evalDecl(env)(decl), UnitV())
    case exp: RslExp => (env, evalExp(env)(exp))

  override def evalDecl(env: Environment)(d: RslDecl): Environment = d match
    case SumDecl(name, ctors) =>
      env.lookupType(name) match
        case Some(_) => throw EvalError("Redeclaration of type " ++ name)
        case None =>
          val newType = DataT(name, ctors.map(c => (c.name, c)).toMap)
          env.insert(name, newType)


  override def evalBlocks(env: Environment)(bs: List[RslBlock]): (Environment, List[RslVal]) = {
    val initState = (env, List[RslVal]())
    bs.foldLeft(initState) { (lastState, b) =>
      val (env, lastRes) = lastState
      val (newEnv, currExp) = evalBlock(env)(b)
      (newEnv, lastRes :+ currExp)
    }
  }

  override def eval(e: RslExp): RslVal =
    try
      evalExp(newEnvironment)(e)
    catch {
      case e: EvalError => ErrV("EvalError: " + e.message)
      case e: Throwable => ErrV(s"[${e.getClass.getCanonicalName}] ${e.getMessage}")
    }

  override def evalProgram(program: RslProgram): List[RslVal] =
    try {
      evalBlocks(Environment(ResilEnv[RslType](), ResilEnv[RslVal]()))(program.blocks)._2
    } catch {
        case e: EvalError => List(ErrV("EvalError: " + e.message))
        case e: Throwable => List(ErrV(e.getMessage))
      }

}