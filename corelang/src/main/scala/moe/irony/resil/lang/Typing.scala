package moe.irony.resil.lang

import moe.irony.resil.sig
import moe.irony.resil.sig.{AUnit, ArrayT, B, Binary, Binop, BoolT, Call, CallDyn, Components, Data, DataT, Env, Fst, Func, FuncT, I, If, IntT, IntV, IsAPair, Letrec, ListT, Logop, Pair, PairT, ParamT, AList, RecordT, Ref, RefT, RslExp, RslType, S, Snd, StrT, Struct, Subscript, TagT, TupleT, UnitT, Update, VarT, Variable}
import moe.irony.resil.utils.IdentifierGenerator

import Console.{RED, BLUE, RESET}

trait ITyping:
  def resolve(typ: RslType): RslType
  def typecheck (exp: RslExp): Either[String, RslType]

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


  private def emptyEnv: Env[RslType] = ResilEnv[RslType]()


  def typeToString (ty: RslType): String = ty match
    case DataT(name, _) => name
    case TagT(name) => s"@$name"
    case TupleT(types) => s"(${types.map(typeToString).mkString(", ")})"
    case RecordT(header, types) => (header match
      case Some(value) => s"$value "
      case None => ""
      ) ++ s"{ ${types.map(typeToString).mkString(", ")} }"
    case ListT(ty) => "list[" ++ typeToString(ty) ++ "]"
    case ArrayT(ty) => "array[" ++ typeToString(ty) ++ "]"
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
    case _ => throw NotImplementedError("Unknown type " ++ ty.toString)

  def optTypeToString (opt: Option[RslType]): String = opt match
    case Some(value) => typeToString(value)
    case None => "_"

  def getConstraints (env: Env[RslType]) (exp: RslExp) (outerCons: List[(RslType, RslType)]): (RslType, List[(RslType, RslType)]) =
    exp match
      case Data(ctorName, fields) => ???
      case Components(values, _) =>
        val t = newVarType
        val allConstraints = values.map(getConstraints(env)(_)(outerCons))
        val ts = allConstraints.map(_._1)
        val cons = allConstraints.flatMap(_._2)
        val allCons = (t, TupleT(ts)) :: cons
        (t, allCons)
      case Struct(header, values) =>
        // Do we need to recheck the correspondences of field names and field types?
        val t = newVarType
        val allFieldConstraints = values.values.map(getConstraints(env)(_)(outerCons)).toList
        val ts = allFieldConstraints.map(_._1)
        val cons = allFieldConstraints.flatMap(_._2)
        val allCons = (t, RecordT(header, ts)) :: cons
        (t, allCons)
      case AList(exps) =>
        val t = newVarType
        val allFieldConstraints = exps.map(getConstraints(env)(_)(outerCons)).toList
        val allCons = allFieldConstraints.map(_._1).map((t, _)) ++ allFieldConstraints.flatMap(_._2)
        // Solve constraints and stop propagation
        (allCons ++ outerCons).reverse.foreach(unify(_)(_))
        (ListT(t), List())
      case sig.Array(elements) =>
        val t = newVarType
        val allFieldConstraints = elements.map(getConstraints(env)(_)(outerCons)).toList
        val allCons = allFieldConstraints.map(_._1).map((t, _)) ++ allFieldConstraints.flatMap(_._2)
          // Solve constraints and stop propagation
        (allCons ++ outerCons).reverse.foreach(unify(_)(_))
        (ArrayT(t), List())
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
        val (t1, cons1) = getConstraints(env)(left)(outerCons)
        val (t2, cons2) = getConstraints(env)(right)(outerCons)
        val allCons = (t, IntT) :: (t1, IntT) :: (t2, IntT) :: (cons1 ++ cons2)
        (t, allCons)
      case Logop(op, left, right) =>
        val (t1, cons1) = getConstraints (env) (left)(outerCons)
        val (t2, cons2) = getConstraints (env) (right)(outerCons)
        val allCons = (t1, t2) :: (cons1 ++ cons2)
        (BoolT, allCons)
      case If(cond, caseTrue, caseElse) =>
        val t = newVarType
        val (t1, cons1) = getConstraints (env) (cond) (outerCons)
        val (t2, cons2) = getConstraints (env) (caseTrue) (outerCons)
        val (t3, cons3) = getConstraints (env) (caseElse) (outerCons)
        val allCons = (t, t2) :: (t, t3) :: (t2, t3) :: (t1, BoolT) :: (cons1 ++ cons2 ++ cons3)
        (t, allCons)
      case Func(arg, body) =>
        val t1 = newVarType
        val (t2, cons) = getConstraints (env.insert(arg, t1)) (body) (outerCons)
        (FuncT(t1, t2), cons)
      case Call(funExp, actual) =>
        val x1 = newVarType
        val (t1, cons1) = getConstraints (env) (funExp) (outerCons)
        val (t2, cons2) = getConstraints (env) (actual) (outerCons)
        val allCons = (t1, FuncT(t2, x1)) :: (cons1 ++ cons2)
        // Solve constraints and stop propagation
        (allCons ++ outerCons).reverse.foreach(unify(_)(_))
        (x1, List())
      case CallDyn(methodName, arg) => throw TypeError("dynamic call is not supported for typing yet")
      case Letrec(assigns, body) =>
        val t = newVarType
        val z = (List[((String, RslType), List[(RslType, RslType)])](), emptyEnv)

        val backfields = assigns.backingField
        val (consList, uncheckedEnvs) = backfields.foldLeft(z) { (zr, sv) =>
          val (s, v) = sv
          val (acc, envs) = zr
          val (ty, list) = getConstraints(envs)(v) (outerCons)
          val newVar = newVarType
          val constraints = ((s, ty), (ty, newVar) :: list) :: acc
          val newEnv: Env[RslType] = envs.insert(s, ty)
          (constraints, newEnv)
        }
        val newEnv = uncheckedEnvs ++ env
        val newCons = consList.foldLeft(List[(RslType, RslType)]()) { (accCons, _cx) =>
          val (_, cx) = _cx
           accCons ++ cx
        }
        val (t1, cons1) = getConstraints (newEnv) (body) (newCons)
        val allCons = (t, t1) :: cons1 ++ newCons
        (t, allCons)
      case Pair(first, second) =>
        val t = newVarType
        val (t2, cons1) = getConstraints (env) (first) (outerCons)
        val (t3, cons2) = getConstraints (env) (second) (outerCons)
        val allCons = (t, PairT(t2, t3)) :: cons1 ++ cons2
        (t, allCons)
      case IsAPair(value) =>
        val t1 = newVarType
        val t2 = newVarType
        val (t3, cons) = getConstraints (env) (value) (outerCons)
        val allCons = (t3, PairT(t1, t2)) :: cons
        (BoolT, allCons)
      case Fst(value) =>
        val t1 = newVarType
        val t2 = newVarType
        val (t3, cons) = getConstraints (env) (value) (outerCons)
        val allCons = (t3, PairT(t1, t2)) :: cons
        (t1, allCons)
      case Snd(value) =>
        val t1 = newVarType
        val t2 = newVarType
        val (t3, cons) = getConstraints (env) (value) (outerCons)
        val allCons = (t3, PairT(t1, t2)) :: cons
        (t2, allCons)
      case AUnit() => (UnitT, List())
      case _ => throw NotImplementedError("Typing.getConstraint unknown expression: " ++ Resil().showExp(exp))
      // TODO: split Resil() to several objects to prevent circular dependency

  def unify (left: RslType) (right: RslType): Unit = (left, right) match
    case (DataT(name1, fields1), DataT(name2, fields2)) => throw NotImplementedError()
    case (TagT(tag1), TagT(tag2)) => throw NotImplementedError()
    case (TupleT(types1), TupleT(types2)) =>
      if types1.size != types2.size then
        throw TypeError("Mismatch tuple sizes")
      else
        types1.zip(types2).foreach { (l, r) =>
          unify (l) (r)
        }
    case (RecordT(header1, types1), RecordT(header2, types2)) =>
      if header1 != header2 then
        throw TypeError("Mismatch record names")
      else if types1.size != types2.size then
        throw TypeError("Mismatch record sizes")
      else
        types1.zip(types2).foreach { (l, r) =>
          unify (l) (r)
        }
    case (ListT(ty1), ListT(ty2)) => 
      if ty1 == ty2 then ()
      else
        throw TypeError("List inner types mismatch")
    case (ArrayT(ty1), ArrayT(ty2)) =>
      if ty1 == ty2 then ()
      else
        throw TypeError("Array inner types mismatch")
    case (RefT(ty1), RefT(ty2)) => throw NotImplementedError()
    case (IntT, IntT) => ()
    case (BoolT, BoolT) => ()
    case (StrT, StrT) => ()
    case (UnitT, UnitT) => ()
    case (PairT(p1, p2), PairT(q1, q2)) =>
      unify (p1) (q1)
      unify (p2) (q2)
    case (FuncT(f1, r1), FuncT(f2, r2))  =>
      unify (f1) (f2)
      unify (r1) (r2)
    case (r @ VarT(i, rf), s @ VarT(j, sf))  =>
      if i == j then
        if rf == sf then ()
        else
          throw TypeError(s"Duplicated type variable #$i")
      else
        (rf, sf) match
          case (None, None) =>
            val newPara = newParamType
            r.inner = Some (newPara)
            s.inner = Some (newPara)
          case (ty: Some[RslType], None) =>
            s.inner = ty
          case (None, ty: Some[RslType]) =>
            r.inner = ty
          case (t1 @ Some(ty1), t2 @ Some(ty2)) =>
            (ty1, ty2) match
              case (ParamT(s1), ParamT(s2)) =>
                if s1 == s2 then ()
                else
                  r.inner = t2
              // Right side concrete types
              case (ParamT(_), DataT(name, ctors)) => throw NotImplementedError()
              case (ParamT(_), TagT(name)) => throw NotImplementedError()
              case (ParamT(_), TupleT(types)) => r.inner = t2
              case (ParamT(_), RecordT(header, types)) => r.inner = t2
              case (ParamT(_), ListT(_)) => r.inner = t2
              case (ParamT(_), ArrayT(_)) => r.inner = t2
              case (ParamT(_), RefT(ty))=> throw NotImplementedError()
              case (ParamT(_), IntT) => r.inner = t2
              case (ParamT(_), BoolT) => r.inner = t2
              case (ParamT(_), StrT) => r.inner = t2
              case (ParamT(_), UnitT) => r.inner = t2
              case (ParamT(_), PairT(_, _)) => r.inner = t2
              case (ParamT(_), FuncT(_, _)) => r.inner = t2
              // Left side concrete types
              case (DataT(name, ctors), ParamT(_)) => throw NotImplementedError()
              case (TagT(name), ParamT(_)) => throw NotImplementedError()
              case (TupleT(types), ParamT(_)) => s.inner = t1
              case (RecordT(header, types), ParamT(_)) => s.inner = t1
              case (ListT(_), ParamT(_)) => s.inner = t1
              case (ArrayT(_), ParamT(_)) => s.inner = t1
              case (RefT(ty), ParamT(_)) => throw NotImplementedError()
              case (IntT, ParamT(_)) => s.inner = t1
              case (BoolT, ParamT(_)) => s.inner = t1
              case (StrT, ParamT(_)) => s.inner = t1
              case (UnitT, ParamT(_)) => s.inner = t1
              case (PairT(_, _), ParamT(_)) => s.inner = t1
              case (FuncT(_, _), ParamT(_)) => s.inner = t1
              // Not concerned, proceed to unification of what it holds
              case _ => unify (ty1) (ty2)
    case (v @ VarT(_, rf), _) => rf match
      case None =>
        if containsType (right) (left) then
          throw TypeError("Type " ++ typeToString(right) ++ " contains type " ++ typeToString(left))
        else
          v.inner = Some(right)
      case Some(ty) =>
        if ty == right then ()
        else unify (ty) (right)
    case (_, v @ VarT(_, rf)) => rf match
      case None =>
        if containsType (left) (right) then
          throw TypeError("Type " ++ typeToString(left) ++ " contains type " ++ typeToString(right))
        else
          v.inner = Some(left)
      case Some(ty) =>
        if ty == left then ()
        else unify (ty) (left)
    case (ParamT(s1), ParamT(s2))  =>
      if s1 == s2
      then ()
      else throw TypeError("Param clash with " ++ s1 ++ " and " ++ s2)
    case _ =>
      throw TypeError("Type check error with " ++ RED ++ typeToString(left) ++ RESET ++ " and " ++ RED ++ typeToString(right) ++ RESET)

  def containsType (outer: RslType) (inner: RslType): Boolean = outer match
    case DataT(name, ctors) => throw NotImplementedError()
    case TagT(name) => throw NotImplementedError()
    case TupleT(types) =>
      types.map(containsType(_)(inner)).reduce(_ || _)
    case RecordT(_, types) =>
      types.map(containsType(_)(inner)).reduce(_ || _)
    case ListT(ty) => containsType(ty)(inner)
    case ArrayT(ty) => containsType(ty)(inner)
    case RefT(ty) => throw NotImplementedError()
    case IntT => false
    case BoolT => false
    case StrT => false
    case UnitT => false
    case PairT(t1, t2) =>
      containsType (t1) (outer) || containsType (t2) (inner)
    case FuncT(t1, t2) =>
      containsType (t1) (outer) || containsType (t2) (inner)
    case VarT(i, rf) => rf match
      case Some(value) => inner match
        case VarT (j, _) => i == j
        case _ => containsType (value) (inner)
      case None => inner match
        case VarT (j, _) => i == j
        case _ => false
    case ParamT(par) => inner match
      case ParamT(p2) => par == p2
      case _ => false
    case _ => throw NotImplementedError("Typing.containsType called on unknown outer type: " ++ typeToString(outer))

  override def resolve(typ: RslType): RslType = typ match
    case DataT(name, ctors) => throw NotImplementedError()
    case TagT(name) => throw NotImplementedError()
    case TupleT(types) =>
      TupleT(types.map(resolve))
    case RecordT(header, types) =>
      RecordT(header, types.map(resolve))
    case ListT(ty) => ListT(resolve(ty))
    case ArrayT(ty) => ArrayT(resolve(ty))
    case RefT(ty) => throw NotImplementedError()
    case IntT => IntT
    case BoolT => BoolT
    case StrT => StrT
    case UnitT => UnitT
    case PairT(t1, t2) => PairT(resolve (t1), resolve (t2))
    case FuncT(arg, res) => FuncT(resolve (arg), resolve (res))
    case v @ VarT(_, rf) => rf match
      case Some(t) => resolve (t)
      case None =>
        val newPara = newParamType
        v.inner = Some(newPara)
        newPara
    case ParamT(_) => typ
    case _ => throw NotImplementedError("Typing.resolve called on unknown type: " ++ typeToString(typ))

  override def typecheck(exp: RslExp): Either[String, RslType] =
    try
      val (t, cons) = getConstraints(emptyEnv) (exp) (List())
      cons.reverse.foreach { (l, r) => unify(l)(r) }
      val typ = resolve(t)
      resetParamType()
      resetVarType()
      println("Type inferred: [ " ++ BLUE ++ typeToString(typ) ++ RESET ++ " ]")
      Right(typ)
    catch
      case e: TypeError =>
        println("Type check failed, reason: " ++ e.message)
        Left(e.message)
      case e: Throwable =>
        println(e.getMessage)
        Left(e.getMessage)

class TypeError(val message: String) extends Exception(message)