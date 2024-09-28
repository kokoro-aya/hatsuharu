package moe.irony.resil

import moe.irony.resil.lang.{Resil, ResilEnv}
import moe.irony.resil.sig.{Ctor, DataDecl, Environment, IntT, IntV, ParamT, RslType, RslVal, TagT, VarT}

@main
def main(): Unit = {

  val evalDecl = (d: DataDecl) => Resil().evalDecl(Environment(ResilEnv[RslType](), ResilEnv[RslVal]()))(d)

  val decl1 = evalDecl(DataDecl("Shape",
    List(
      Ctor("Square", Map("size" -> IntT)),
      Ctor("Circle", Map("perimeter" -> IntT)),
      Ctor("Rectangle", Map("width" -> IntT, "height" -> IntT)),
      Ctor("Diamond", Map("size" -> IntT, "angle" -> IntT))
    )))

  val decl2 = evalDecl(DataDecl("Animal",
    List(
      Ctor("Square", Map("size" -> IntT)),
      Ctor("Circle", Map("perimeter" -> IntT)),
      Ctor("Rectangle", Map("width" -> IntT, "height" -> IntT)),
      Ctor("Diamond", Map("size" -> IntT, "angle" -> IntT))
    )))


  val decl3 = evalDecl(DataDecl("Animal",
    List(
      Ctor("Square", Map("size" -> IntT)),
      Ctor("Circle", Map("perimeter" -> IntT)),
      Ctor("Rectangle", Map("width" -> IntT, "height" -> IntT)),
      Ctor("Diamond", Map("size" -> IntT, "angle" -> IntT))
    )))

  val evalBlocks = Resil().evalBlocks(Environment(ResilEnv[RslType](), ResilEnv[RslVal]()))

  val blocks = evalBlocks(List(
    DataDecl("Nat",
      List(
        Ctor("S", Map("" -> TagT("Nat"))),
        Ctor("O", Map())
      )
    ),
    DataDecl("Bool",
      List(
        Ctor("True", Map()),
        Ctor("False", Map())
      )
    ),
    DataDecl("Data",
      List(
        Ctor("NatData", Map("value" -> TagT("Nat"))),
        Ctor("BoolData", Map("value" -> TagT("Bool")))
      )
    ),
  ))

  println("done.")
}