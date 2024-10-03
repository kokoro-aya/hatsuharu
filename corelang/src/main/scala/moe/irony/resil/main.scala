package moe.irony.resil

import moe.irony.resil.lang.{Resil, ResilEnv, Typing}
import moe.irony.resil.sig.Binary.{ADD, MULT}
import moe.irony.resil.sig.{AList, AUnit, Binop, Call, Ctor, DataDecl, Environment, Func, I, IntT, IntV, Letrec, Nth, Pair, ParamT, RslExp, RslType, RslVal, S, Size, Snd, TagT, VarT, Variable}


def mkEnv(bindings: (String, RslExp)*): ResilEnv[RslExp] = {
  ResilEnv[RslExp](bindings.toList)
}

@main
def main(): Unit = {

  val letrec5 = Letrec(
    mkEnv(
      ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
      ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
      ("z", I(3))
    ),
    (Call(Variable("f"), Call(Variable("g"), Variable("z"))))
  )

//  val exp5 = Call(Func("x", Binop(ADD, Variable("x"), I(1))),
//    Call(Func("x", Binop(MULT, Variable("x"), I(2))), I(3)))
  val ty1 = Typing().typecheck(letrec5)
//  val ty2 = Typing().typecheck(exp5)

  println(ty1)
//  println(ty2)
}

/*
      f : 'a -> 'b

      double : f f x

      (f x) : 'b
      f (f x) : 'a -> 'b
 */
