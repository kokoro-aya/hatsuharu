package moe.irony.resil.lang

import moe.irony.resil.sig
import moe.irony.resil.sig.{AUnit, ArrayT, B, Binary, Binop, BoolT, Call, CallDyn, Components, Data, DataT, Env, Fst, Func, FuncT, I, If, IntT, IntV, IsAPair, Letrec, ListT, Logop, Pair, PairT, ParamT, ReadonlyList, RecordT, Ref, RefT, RslExp, RslType, S, Snd, StrT, Struct, Subscript, TagT, TupleT, UnitT, Update, VarT, Variable}
import moe.irony.resil.utils.IdentifierGenerator

trait ITyping:
  def resolve(typ: RslType): RslType
  def typecheck (exp: RslExp): Unit

class Typing extends ITyping:

  val paramCharCount: IdentifierGenerator = IdentifierGenerator(26)
  val varCount: IdentifierGenerator = IdentifierGenerator(26)

  def newParamType: RslType =
    ParamT(paramCharCount.nextId)

  def resetParamType(): Unit =
    paramCharCount.reset

  def newVarType: RslType =
    VarT(varCount.nextNum, None)

  def resetVarType(): Unit =
    varCount.reset


  def typeToString (ty: RslType): String = ty match
    case DataT(name, _) => name
    case TagT(name) => s"@$name"
    case TupleT(types) => s"(${types.map(typeToString).mkString(", ")})"
    case RecordT(header, types) => (header match
      case Some(value) => s"$value "
      case None => ""
      ) ++ s"{ ${types.map(typeToString).mkString(", ")} }"
    case ListT() => "list"
    case ArrayT() => "array"
    case RefT(ty) => s"&${typeToString(ty)}"
    case IntT => "int"
    case BoolT => "bool"
    case StrT => "string"
    case UnitT => "unit"
    case PairT(t1, t2) => s"(${typeToString(t1)}, ${typeToString(t2)})"
    case FuncT(arg, res) => s"(${typeToString(arg)} => ${typeToString(res)})"
    case ParamT(param) => s"?$param"
    case VarT(count, inner) => s"X$count:${
      inner match
        case Some(ity) => typeToString(ity)
        case None => "_"
    }"

  def optTypeToString (opt: Option[RslType]): String = opt match
    case Some(value) => typeToString(value)
    case None => "_"




  def getConstraints (env: Env[RslType]) (exp: RslExp): (RslType, List[(RslType, RslType)]) =
    exp match
      case Data(header, fields) => ???
      case Components(values, arity) => ???
      case Struct(header, values) => ???
      case ReadonlyList(values) => ???
      case sig.Array(elements) => ???
      case Ref(value) => ???
      case Update(assignee, assigned) => ???
      case Subscript(value, subscript) => ???
      case I(_) => (IntT, List())
      case B(_) => (BoolT, List())
      case S(_) => (StrT, List())
      case Variable(label) => env.lookup(label) match
        case Some(value) => (value, List())
        case None => throw TypeError("Unknown variable " ++ label)
      case Binop(op, left, right) =>
        val t = newVarType
        val (t1, cons1) = getConstraints(env)(left)
        val (t2, cons2) = getConstraints(env)(right)
        val allCons = (t, t1) :: (t1, t2) :: (t2, IntT) :: (cons1 ++ cons2)
        (t, allCons)
      case Logop(op, left, right) =>
        val t = newVarType
        val (t1, cons1) = getConstraints (env) (left)
        val (t2, cons2) = getConstraints (env) (right)
        val allCons = (t, BoolT) :: (t1, t2) :: (cons1 ++ cons2)
        (t, allCons)
      case If(cond, caseTrue, caseElse) =>
        val t = newVarType
        val (t1, cons1) = getConstraints (env) (cond)
        val (t2, cons2) = getConstraints (env) (caseTrue)
        val (t3, cons3) = getConstraints (env) (caseElse)
        val allCons = (t, t2) :: (t, t3) :: (t2, t3) :: (t1, BoolT) :: (cons1 ++ cons2 ++ cons3)
        (t, allCons)
      case Func(arg, body) =>
        val t1 = newVarType
        val (t2, cons) = getConstraints (env.insert(arg, t1)) (body)
        (FuncT(t1, t2), cons)
      case Call(funExp, actual) =>
        val x1 = newVarType
        val x2 = newVarType
        val (t1, cons1) = getConstraints (env) (funExp)
        val (t2, cons2) = getConstraints (env) (actual)
        val allCons = (t1, FuncT(x1, x2)) :: (t2, x1) :: (cons1 ++ cons2)
        (x2, allCons)
      case CallDyn(methodName, arg) => throw TypeError("dynamic call is not supported for typing yet")
      case Letrec(assigns, body) =>
        def emptyEnv: Env[RslType] = ResilEnv[RslType]()

        val t = newVarType
        val z = (List[((String, RslType), List[(RslType, RslType)])](), emptyEnv)

        val backfields = assigns.backingField
        val (consList, uncheckedEnvs) = backfields.foldLeft(z) { (zr, sv) =>
          val (s, v) = sv
          val (acc, envs) = zr
          val (ty, list) = getConstraints(envs)(v)
          val newVar = newVarType
          val constraints = ((s, ty), (ty, newVar) :: list) :: acc
          val newEnv: Env[RslType] = envs.insert(s, ty)
          (constraints, newEnv)
        }

        val newEnv = uncheckedEnvs ++ env
        val newCons = consList.foldLeft(List[(RslType, RslType)]()) { (accCons, _cx) =>
          val (_, cx) = _cx
          cx ++ accCons
        }

        val (t1, cons1) = getConstraints (newEnv) (body)
        val allCons = (t, t1) :: cons1 ++ newCons
        (t, allCons)
      case Pair(first, second) =>
        val t = newVarType
        val (t2, cons1) = getConstraints (env) (first)
        val (t3, cons2) = getConstraints (env) (second)
        val allCons = (t, PairT(t2, t3)) :: cons1 ++ cons2
        (t, allCons)
      case IsAPair(value) =>
        val t1 = newVarType
        val t2 = newVarType
        val (t3, cons) = getConstraints (env) (value)
        val allCons = (t3, PairT(t1, t2)) :: cons
        (BoolT, allCons)
      case Fst(value) =>
        val t1 = newVarType
        val t2 = newVarType
        val (t3, cons) = getConstraints (env) (value)
        val allCons = (t3, PairT(t1, t2)) :: cons
        (newVarType, allCons)
      case Snd(value) =>
        val t1 = newVarType
        val t2 = newVarType
        val (t3, cons) = getConstraints (env) (value)
        val allCons = (t3, PairT(t1, t2)) :: cons
        (newVarType, allCons)
      case AUnit() => (UnitT, List())

  def unify (left: RslType) (right: RslType): Unit = ???

  def containsType (typ1: RslType) (typ2: RslType): Boolean = ???

  override def resolve(typ: RslType): RslType = ???

  override def typecheck(exp: RslExp): Unit = ???

class TypeError(val message: String) extends Exception(message)