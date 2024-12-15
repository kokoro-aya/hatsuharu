package moe.irony.resil

import moe.irony.resil.lang.{Resil, ResilEnv, Typing, newEnvironment}
import moe.irony.resil.sig.Binary.{ADD, MULT, SUB}
import moe.irony.resil.sig.Logical.{EQ, LT}
import moe.irony.resil.sig.{AList, AUnit, ActualMethodDecl, AnyClass, B, Binary, Binop, BoolT, Call, ClassBlock, ClassDecl, ClassTreeNode, Components, Ctor, CtorPattern, Data, Environment, Func, FuncT, I, If, InstanceDecl, IntT, IntV, Letrec, ListPattern, Logop, Match, Nth, Pair, ParamT, RecordPattern, RecordV, RslBlock, RslClassTree, RslDecl, RslExp, RslPattern, RslType, RslTypedVar, RslVal, RslVar, S, Size, Snd, StrT, Struct, SumDecl, TagT, TuplePattern, TypeParamT, TypeVarT, VarT, Variable, VirtualMethodDecl, WildcardPattern}

import scala.collection.immutable.List
import scala.collection.mutable

object Examples {

  def sampleSource: List[RslExp] = {
    /*
              double = \f -> \x -> f (f x)
              f = \x -> x + 1
              g = \x -> x * 2
              z = 3

              double (\x -> f (g x)) z
        */
    val doubleFun = Letrec(
      mkEnv(
        ("double", Func("f", Func("x", Call(Variable("f"), Call(Variable("f"), Variable("x")))))),
        ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
        ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
        ("z", I(3))
      ),
      Call(Call(Variable("double"), Func("x", Call(Variable("f"), Call(Variable("g"), Variable("x"))))), Variable("z"))
    )

    val listFun = AList(List(
      Binop(Binary.SUB, Binop(Binary.ADD, I(3), I(5)), I(9)),
      Call(Func("x", Binop(MULT, Variable("x"), I(4))), I(5)),
      I(10)
    ))

    val expWrong1 = AList(List(
      Func("y", Binop(MULT, Variable("y"), I(3))),
      Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y")))),
      Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y"))))
    ))

    val expWrong2 = AList(List(
      Func("x", Func("y", Logop(LT, Variable("y"), Variable("x")))),
      Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y")))),
      Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y"))))
    ))

    val expOK = AList(List(
      Func("x", Func("y", Binop(ADD, Variable("x"), Variable("y")))),
      Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y"))))
    ))

    //  val f = fn a => fn b => a + b
    //  val g = fn x => fn y => x * y
    //  val abc = fn x  => (f x)
    //  val def = fn x => fn y => (g x) y
    val expFnList =
      Letrec(
        mkEnv(
          ("f", Func("a", Func("b", Binop(MULT, Variable("a"), Variable("b"))))),
          ("g", Func("x", Func("y", Binop(ADD, Variable("x"), Variable("y"))))),
        ),
        AList(List(
          Variable("f"),
          Func("x", Call(Variable("f"), Variable("x"))),
          Func("x", Func("y", Call(Call(Variable("g"), Variable("x")), Variable("y"))))
        )

        ))

    val arrayExps = sig.Array(mutable.ArraySeq(
      Func("f", Func("g", Func("x", Call(Variable("f"), Call(Variable("f"), Call(Variable("g"), Variable("x"))))))),
      Func("f", Func("g", Variable("g")))
    ))

    val record = sig.Struct(Some("Student"), Map("mark" -> I(4), "grade" -> S("A")))

    List(
      doubleFun, listFun, expWrong1, expWrong2, expOK, expFnList, arrayExps, record
    )
  }

  /**
   * Sample codes to represent typing of minimal language features
   */
  def typingSamples() = {
    sampleSource.foreach { exp =>
      println(Resil().showExp(exp))
      Typing().typecheck(exp)
      println()
    }
  }

  /**
   * Sample codes to represent eval of minimal language features
   */
  def evalSamples() = {
    sampleSource.foreach { exp =>
      println(Resil().showExp(exp))
      val res = Resil().eval(exp)
      println(s"Eval result: ${Resil().show(res)}")
      println()
    }
  }
}