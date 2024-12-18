package moe.irony.resil.sig

import scala.collection.mutable.ArraySeq
import scala.collection.{Map, mutable}

trait Env[A]:

  def insert(label: String, value: A): Env[A]
  def update(label: String) (newValue: A): Env[A]
  def lookup(label: String): Option[A]

  def lookupBy(label: String) (criteria: (A) => Boolean): Option[A]
  def lookupBy(criteria: (A) => Boolean): Option[A]

  infix def ++(other: Env[A]): Env[A]

  def dumpNames: String

  val backingField: List[(String, A)]


case class Environment(var types: Env[RslType], var variables: Env[RslVal]):

  def insert(label: String, ty: RslType): Environment = {
    types = types.insert(label, ty)
    this
  }
  def insert(label: String, v: RslVal): Environment = {
    variables = variables.insert(label, v)
    this
  }

  def update(label: String, v: RslVal): Environment = {
    variables = variables.update(label) (v)
    this
  }

  def lookupType(label: String): Option[RslType] = types.lookup(label)
  def lookupValue(label: String): Option[RslVal] = variables.lookup(label)

  def lookupTypeByCriteria (criteria: (RslType) => Boolean): Option[RslType] =
    types.lookupBy(criteria)
  def lookupValueByCriteria (criteria: (RslVal) => Boolean): Option[RslVal] =
    variables.lookupBy(criteria)

  def +++(other: Env[RslType]): Environment = {
    this.types = this.types ++ other
    this
  }

  def ++(other: Env[RslVal]): Environment = {
    this.variables = this.variables ++ other
    this
  }

  def dumpTypes: String = types.dumpNames
  def dumpValues: String = variables.dumpNames




trait Op

enum Binary extends Op {
  case ADD, SUB, MULT, DIV, MOD
  
  def show(): String = this match
    case Binary.ADD => "+"
    case Binary.SUB => "-"
    case Binary.MULT => "*"
    case Binary.DIV => "/"
    case Binary.MOD => "%"
}

enum Logical extends Op {
  case EQ, LT, LE, NEQ
  
  def show(): String = this match
    case Logical.EQ => "=="
    case Logical.LT => ">"
    case Logical.LE => ">="
    case Logical.NEQ => "!="
}


sealed trait RslVal

case class UnionV(header: String, fields: List[RslVal]) extends RslVal
case class TupleV(values: List[RslVal], arity: Int) extends RslVal
case class RecordV(header: Option[String], values: Map[String, RslVal]) extends RslVal
case class ListV(values: List[RslVal]) extends RslVal
case class ArrayV(var values: mutable.ArraySeq[RslVal], length: Int) extends RslVal
case class RefV(var value: RslVal) extends RslVal

case class IntV(value: Int) extends RslVal
case class BoolV(value: Boolean) extends RslVal
case class StrV(value: String) extends RslVal
case class PairV(first: RslVal, second: RslVal) extends RslVal
case class UnitV() extends RslVal
case class ClosV(env: Env[RslVal], f: RslExp) extends RslVal
case class PromV(var promVal: Option[RslVal]) extends RslVal // Todo: rename this to something else to free "promise" primitve
case class ErrV(errMessage: String) extends RslVal


sealed class RslType

case class DataT(name: String, typeParams: List[RslType], ctors: Map[String, Ctor]) extends RslType
// A simple variant of DataT arm, used for internal typing match
case class VariantT(sumName: String, typeParams: List[RslType], ctor: Ctor) extends RslType
case class TagT(name: String) extends RslType // e.g. "Shape" for `data Shape = Square Int | Circle Int ...`
case class TupleT(types: List[RslType]) extends RslType
case class RecordT(header: Option[String], types: List[(String, RslType)]) extends RslType
case class ListT(inner: RslType) extends RslType
case class ArrayT(inner: RslType) extends RslType
case class RefT(ty: RslType) extends RslType

case class TypeVarT(name: String, var inner: Option[RslType] = None) extends RslType

object IntT extends RslType
object BoolT extends RslType
object StrT extends RslType
object UnitT extends RslType
case class PairT(t1: RslType, t2: RslType) extends RslType
case class FuncT(arg: RslType, res: RslType) extends RslType
// TODO: add comments for intermediate types
case class ParamT(param: String) extends RslType
case class VarT(count: Int, var inner: Option[RslType]) extends RslType

case class TypeParamT(base: String, param: List[RslType]) extends RslType

def copyRslType(ty: RslType): RslType = ty match
  case DataT(name, typeParams, ctors) => DataT(name, typeParams, ctors)
  case VariantT(sumName, typeParams, ctor) => VariantT(sumName, typeParams, ctor)
  case TagT(name) => TagT(name)
  case TupleT(types) => TupleT(types)
  case RecordT(header, types) => RecordT(header, types)
  case ListT(inner) => ListT(inner)
  case ArrayT(inner) => ArrayT(inner)
  case RefT(ty) => RefT(ty)
  case TypeVarT(name, inner) => TypeVarT(name, inner match
    case Some(value) => Some(value)
    case None => None)
  case IntT => IntT
  case BoolT => BoolT
  case StrT => StrT
  case UnitT => UnitT
  case PairT(t1, t2) => PairT(t1, t2)
  case FuncT(arg, res) => FuncT(arg, res)
  case ParamT(param) => ParamT(param)
  case VarT(count, inner) => VarT(count, inner)
  case TypeParamT(base, param) => TypeParamT(base, param)

// case class AnyT
// case class NothingT

sealed trait RslAssignable


sealed class RslSubscript extends RslAssignable

// foo.abc  foo[1]  foo[1].abc  foo.abc[1] = ???
// In the future => replace subscript by full expression
// what about desugar to "set(assign, value)"?

case class NamedSubscript(label: String) extends RslSubscript
case class NumberSubscript(value: Int) extends RslSubscript
case class CompoundSubscript(pattern: RslVar, subscript: RslSubscript) extends RslSubscript
// TODO: Add dot operator ... precedence ?

sealed class RslPattern extends RslAssignable

case class TuplePattern(items: List[RslAssignable]) extends RslPattern   // (a, b)
case class ListPattern(elements: List[RslAssignable]) extends RslPattern // hd :: tl
case class RecordPattern(fields: List[RslVar]) extends RslPattern // { foo, bar }
case class CtorPattern(name: String, fields: List[RslVar]) extends RslPattern // Cat(a, b)
case object WildcardPattern extends RslPattern                        // _

case class RslTypedVar(rslVar: RslAssignable, typ: RslType) extends RslAssignable
case class RslVar(label: String) extends RslAssignable                // foo =    // not to confuse with NamedSubscript

case class RslProgram(blocks: List[RslBlock])


sealed class RslBlock


sealed class RslDecl extends RslBlock


case class Ctor(name: String, fields: List[(String, RslType)])
case class SumDecl(name: String, params: List[RslType], ctors: List[Ctor]) extends RslDecl

sealed class MethodDecl
case class VirtualMethodDecl(name: String, methodType: RslType) extends MethodDecl
case class ActualMethodDecl(name: String, body: RslExp) extends MethodDecl

case class ClassBlock(methods: List[MethodDecl])

case class ClassDecl(name: String, typeParams: List[String], body: ClassBlock) extends RslDecl
case class InstanceDecl(name: String, typeParams: Map[String, List[String]], body: ClassBlock) extends RslDecl


sealed class RslClassTree
case class AnyClass(classes: List[RslClassTree]) extends RslClassTree
case class ClassTreeNode(name: String, params: List[String], subClasses: List[RslClassTree]) extends RslClassTree


//case class FuncDecl(name: String, ) extends RslDecl
//case class ConstDecl extends RslDecl

sealed class RslExp extends RslBlock

case class RslScope(blocks: List[RslBlock]) // TODO: [2]

// TODO: add Lazy construct

// val a = Square(1, 2)
// val b = (a, 12, "3")
// val c1 = { title: "Hello", text: "world" }
// val c2 = Color { r: 120, g: 33, b: 64, alpha: 52 }
// val d = [1, 2, 3, 4, 5]
// val e = array (1, 2, 3, 4, 5)
//     e[3] := 0
// val f = ref (5)
//     f := 4

case class Data(header: String, fields: List[RslExp]) extends RslExp
case class Struct(header: Option[String], values: Map[String, RslExp]) extends RslExp
case class Array(var elements: mutable.ArraySeq[RslExp]) extends RslExp
case class Ref(value: RslExp) extends RslExp
case class Update(assignee: RslExp, assigned: RslExp) extends RslExp 
case class Subscript(value: RslExp, subscript: RslExp) extends RslExp

case class I(value: Int) extends RslExp
case class B(value: Boolean) extends RslExp
case class S(value: String) extends RslExp
case class Variable(label: String) extends RslExp
case class Binop(op: Binary, left: RslExp, right: RslExp) extends RslExp
case class Logop(op: Logical, left: RslExp, right: RslExp) extends RslExp
case class If(cond: RslExp, caseTrue: RslExp, caseElse: RslExp) extends RslExp
case class Func(param: String, body: RslExp) extends RslExp
case class Call(fn: RslExp, arg: RslExp) extends RslExp
case class CallDyn(methodName: RslExp, arg: RslExp) extends RslExp
case class Letrec(env: List[(RslAssignable, RslExp)], body: RslExp) extends RslExp
case class Match(exp: RslExp, cases: List[(RslPattern, RslExp)]) extends RslExp
case class Pair(first: RslExp, second: RslExp) extends RslExp
case class IsAPair(value: RslExp) extends RslExp
case class Fst(value: RslExp) extends RslExp
case class Snd(value: RslExp) extends RslExp
case class Components(values: List[RslExp], arity: Int) extends RslExp
case class NthComponent(values: RslExp, pos: RslExp) extends RslExp
case class AList(values: List[RslExp]) extends RslExp
case class Head(value: RslExp) extends RslExp
case class Tail(value: RslExp) extends RslExp
case class Nth(coll: RslExp, idx: RslExp) extends RslExp
case class Size(value: RslExp) extends RslExp
case class IsEmpty(value: RslExp) extends RslExp
case class AUnit() extends RslExp


class EvalError(val message: String) extends Exception(message)

trait Rsl:

  // TODO: refactor to extension methods
  def show (v: RslVal): String

  def typ (v: RslVal): String

  def evalExp (env: Environment) (e: RslExp): RslVal
  def evalDecl (env: Environment) (d: RslDecl): Environment

  def evalBlock (env: Environment) (b: RslBlock): (Environment, RslVal)

  def evalBlocks (env: Environment) (bs: List[RslBlock]): (Environment, List[RslVal])

  def eval (e: RslExp): RslVal

  def evalProgram (program: RslProgram): List[RslVal]
