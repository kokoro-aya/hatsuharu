package moe.irony.resil

import moe.irony.resil.lang.{Resil, ResilEnv, Typing}
import moe.irony.resil.sig.Binary.{ADD, MULT}
import moe.irony.resil.sig.Logical.LT
import moe.irony.resil.sig.{AList, AUnit, Binary, Binop, Call, Ctor, DataDecl, Environment, Func, I, IntT, IntV, Letrec, Logop, Nth, Pair, ParamT, RslExp, RslType, RslVal, S, Size, Snd, TagT, VarT, Variable}

import scala.collection.mutable


def mkEnv(bindings: (String, RslExp)*): ResilEnv[RslExp] = {
  ResilEnv[RslExp](bindings.toList)
}

@main
def main(): Unit = {
//  /*
//        double = \f -> \x -> f (f x)
//        f = \x -> x + 1
//        g = \x -> x * 2
//        z = 3
//
//        double (\x -> f (g x)) z
//  */
////  val exp = Letrec(
////    mkEnv(
////      ("double", Func("f", Func("x", Call(Variable("f"), Call(Variable("f"), Variable("x")))))),
////      ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
////      ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
////      ("z", I(3))
////    ),
////    Call(Call(Variable("double"), Func("x", Call(Variable("f"), Call(Variable("g"), Variable("x"))))), Variable("z"))
////  )
////  Typing().typecheck(exp)
////
////  val exp = AList(List(
////    Binop(Binary.SUB, Binop(Binary.ADD, I(3), I(5)), I(9)),
////    Call(Func("x", Binop(MULT, Variable("x"), I(4))), I(5)),
////    I(10)
////  ))
//
//  val expWrong1 = AList(List(
//    Func("y", Binop(MULT, Variable("y"), I(3))),
//    Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y")))),
//    Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y"))))
//  ))
//
//  val expWrong2 = AList(List(
//    Func("x", Func("y", Logop(LT, Variable("y"), Variable("x")))),
//    Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y")))),
//    Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y"))))
//  ))
//
//  val expOK = AList(List(
//    Func("x", Func("y", Binop(ADD, Variable("x"), Variable("y")))),
//    Func("x", Func("y", Binop(MULT, Variable("x"), Variable("y"))))
//  ))
//
//  //  val f = fn a => fn b => a + b
//  //  val g = fn x => fn y => x * y
//  //  val abc = fn x  => (f x)
//  //  val def = fn x => fn y => (g x) y
//  val expFnList =
//    Letrec(
//      mkEnv(
//        ("f", Func("a", Func("b", Binop(MULT, Variable("a"), Variable("b"))))),
//        ("g", Func("x", Func("y", Binop(ADD, Variable("x"), Variable("y"))))),
//      ),
//      AList(List(
//        Variable("f"),
//        Func("x", Call(Variable("f"), Variable("x"))),
//        Func("x", Func("y", Call(Call(Variable("g"), Variable("x")), Variable("y"))))
//      )
//
//  ))
//
//  val arrayExps = sig.Array(mutable.ArraySeq(
//    Func("f", Func("g", Func("x", Call(Variable("f"), Call(Variable("f"), Call(Variable("g"), Variable("x"))))))),
//    Func("f", Func("g", Variable("g")))
//  ))
//
//  val record = sig.Struct(Some("Student"), Map("mark" -> I(4), "grade" -> S("A")))
//
//
//  println(Resil().showExp(expWrong1))
//  Typing().typecheck(expWrong1)
//  println()
//
//  println(Resil().showExp(expWrong2))
//  Typing().typecheck(expWrong2)
//  println()
//
//  println(Resil().showExp(expOK))
//  Typing().typecheck(expOK)
//  println()
//
//  println(Resil().showExp(expFnList))
//  Typing().typecheck(expFnList)
//  println()
//
//  println(Resil().showExp(arrayExps))
//  Typing().typecheck(arrayExps)
//  println()
//
//  println(Resil().showExp(record))
//  Typing().typecheck(record)
//  println()

  val exp = Letrec(
    mkEnv(
      ("f", Func("x", Call(Variable("f"), Variable("x"))))
    ),
    Variable("f")
  )

  Typing().typecheck(exp)
  println()

}