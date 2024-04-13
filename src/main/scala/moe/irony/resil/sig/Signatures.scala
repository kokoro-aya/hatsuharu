package moe.irony.resil.sig


trait Env[A]:

  def insert(label: String, value: A): Env[A]
  def update(label: String) (newValue: A): Env[A]
  def lookup(label: String): Option[A]

  def lookupBy(label: String) (criteria: (A) => Boolean): Option[A]

  infix def ++(other: Env[A]): Env[A]

  def dumpNames: String

  val backingField: List[(String, A)]

trait Op

enum Binary extends Op {
  case ADD, SUB, MULT, DIV, MOD
}

enum Logical extends Op {
  case EQ, LT, LE, NEQ
}


sealed trait RslVal

case class IntV(value: Int) extends RslVal
case class BoolV(value: Boolean) extends RslVal
case class StrV(value: String) extends RslVal
case class PairV(first: RslVal, second: RslVal) extends RslVal
case class UnitV() extends RslVal
case class ClosV(env: Env[RslVal], f: RslExp) extends RslVal
case class PromV(var promVal: Option[RslVal]) extends RslVal
case class ErrV(errMessage: String) extends RslVal

sealed class RslExp

case class I(value: Int) extends RslExp
case class B(value: Boolean) extends RslExp
case class S(value: String) extends RslExp
case class Var(label: String) extends RslExp
case class Binop(op: Binary, left: RslExp, right: RslExp) extends RslExp
case class Logop(op: Logical, left: RslExp, right: RslExp) extends RslExp
case class If(cond: RslExp, caseTrue: RslExp, caseElse: RslExp) extends RslExp
case class Func(param: String, body: RslExp) extends RslExp
case class Call(fn: RslExp, arg: RslExp) extends RslExp
case class CallDyn(methodName: RslExp, arg: RslExp) extends RslExp
case class Letrec(env: Env[RslExp], body: RslExp) extends RslExp
case class Pair(first: RslExp, second: RslExp) extends RslExp
case class IsAPair(value: RslExp) extends RslExp
case class Fst(value: RslExp) extends RslExp
case class Snd(value: RslExp) extends RslExp
case class AUnit() extends RslExp


class EvalError(val message: String) extends Exception(message)

trait Rsl:



  // TODO: refactor to extension methods
  def show (v: RslVal): String

  def typ (v: RslVal): String

  def evalEnv (env: Env[RslVal]) (e: RslExp): RslVal

  def eval (e: RslExp): RslVal