package moe.irony.resil

import moe.irony.resil.lang.{Resil, ResilEnv, Typing, newEnvironment}
import moe.irony.resil.sig.Binary.{ADD, MULT, SUB}
import moe.irony.resil.sig.Logical.{EQ, LT}
import moe.irony.resil.sig.{AList, AUnit, ActualMethodDecl, AnyClass, B, Binary, Binop, BoolT, Call, ClassBlock, ClassDecl, ClassTreeNode, Components, Ctor, CtorPattern, Data, Environment, Func, FuncT, I, If, InstanceDecl, IntT, IntV, Letrec, ListPattern, Logop, Match, Nth, Pair, ParamT, RecordPattern, RecordV, RslBlock, RslClassTree, RslDecl, RslExp, RslPattern, RslType, RslTypedVar, RslVal, RslVar, S, Size, Snd, StrT, Struct, SumDecl, TagT, TuplePattern, TypeParamT, TypeVarT, VarT, Variable, VirtualMethodDecl, WildcardPattern}

import scala.collection.immutable.List
import scala.collection.mutable


def mkEnv(bindings: (String, RslExp)*): List[(RslVar, RslExp)] = {
  bindings.map{ (s, e) =>  (RslVar(s), e) }.toList
}

def evalSeveralExps = {
  /*
        double = \f -> \x -> f (f x)
        f = \x -> x + 1
        g = \x -> x * 2
        z = 3

        double (\x -> f (g x)) z
  */
  //  val exp = Letrec(
  //    mkEnv(
  //      ("double", Func("f", Func("x", Call(Variable("f"), Call(Variable("f"), Variable("x")))))),
  //      ("f", Func("x", Binop(ADD, Variable("x"), I(1)))),
  //      ("g", Func("x", Binop(MULT, Variable("x"), I(2)))),
  //      ("z", I(3))
  //    ),
  //    Call(Call(Variable("double"), Func("x", Call(Variable("f"), Call(Variable("g"), Variable("x"))))), Variable("z"))
  //  )
  //  Typing().typecheck(exp)
  //
  //  val exp = AList(List(
  //    Binop(Binary.SUB, Binop(Binary.ADD, I(3), I(5)), I(9)),
  //    Call(Func("x", Binop(MULT, Variable("x"), I(4))), I(5)),
  //    I(10)
  //  ))

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


  println(Resil().showExp(expWrong1))
  Typing().typecheck(expWrong1)
  println()

  println(Resil().showExp(expWrong2))
  Typing().typecheck(expWrong2)
  println()

  println(Resil().showExp(expOK))
  Typing().typecheck(expOK)
  println()

  println(Resil().showExp(expFnList))
  Typing().typecheck(expFnList)
  println()

  println(Resil().showExp(arrayExps))
  Typing().typecheck(arrayExps)
  println()

  println(Resil().showExp(record))
  Typing().typecheck(record)
  println()
}

def eval1() = {
    val exp = Letrec(
      mkEnv(
        ("f", Func("x", Call(Variable("f"), Variable("x"))))
      ),
      Variable("f")
    )

    Typing().typecheck(exp)
    println()
}

def eval2() = {

  val blocks1 =
    List[RslBlock](
      SumDecl(
        "Shape", List(), List(
          Ctor("Square", List("side" -> IntT)),
          Ctor("Circle", List("radius" -> IntT)),
          Ctor("Rectangle", List("width" -> BoolT, "height" -> StrT)) // Type not checked yet
        )),
      Data("Square", List(I(16))),
      Data("Rectangle", List(I(7), I(8)))
    )

  Typing().typecheck(blocks1)

  val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks1)

  res1.map { it => Resil().show(it) }.foreach(println)
}

@main
def main(): Unit = {
//  val expr = Letrec(
//    List(
//      (RslVar("a"), Components(List(I(1), S("str")), 2))
//    ),
//    Variable("a")
//  )
//
//  Typing().typecheck(expr)
//
//  val res = Resil().eval(expr)
//  println(res)

//  val expr = Letrec(
//    List(
//      (TuplePattern(List(RslVar("a"), RslVar("b"))), Components(List(I(1), S("str")), 2))
//    ),
//    Variable("a")
//  )
//
//  Typing().typecheck(expr)
//
//  val res = Resil().eval(expr)
//  println(res)

//  val expr = Letrec(
//    List(
//      (TuplePattern(List(RslVar("a"), RslVar("b"))),
//        Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
//    ),
//    Variable("b")
//  )
//
//  Typing().typecheck(expr)
//
//  val res = Resil().eval(expr)
//  println(res)


//  val expr = Letrec(
//    List(
//      (TuplePattern(List(RslVar("a"), RslVar("b"), RslVar("c"))),
//        Components(List(I(1), Components(List(I(97), S("bar"), B(false)), 3)), 2))
//    ),
//    Variable("b")
//  )

//  val expr = Letrec(
//    List(
//      (ListPattern(List(RslVar("x"), RslVar("xs"))),
//        AList(List(I(1), I(2), I(3), I(4))
//          //          AList(List(I(1), Components(List(I(97), S("bar"), B(false))), 2))
//        ))),
//    Variable("x")
//  )
//
//  Typing().typecheck(expr)
//
//  val res = Resil().eval(expr)
//  println(res)

  // Error: [1,2,3,"S"], [[1,2,3],["S","Z"]]

//  val expr = Letrec(
//    List(
//      (TuplePattern(List(ListPattern(List(RslVar("x"), RslVar("xs"))), RslVar("ys"))),
//        Components(
//          List(
//            (AList(List(I(1), I(2), I(3), I(4)))),
//            (AList(List(I(1), I(2), I(3), I(4))))
//        ), 2))),
//    Variable("ys")
//  )
//
//  Typing().typecheck(expr)
//
//  val res = Resil().eval(expr)
//  println(res)

//  val expr = Letrec(
//    List(
//      (RecordPattern(List(
//        RslVar("a"), RslVar("b"), RslVar("c")
//      )),
//        Struct(None, Map("a" -> I(3), "b" -> I(4), "c" -> B(false))))
//    ),
//    Variable("c")
//  )
//
//  println(Resil().showExp(expr))
//
//  Typing().typecheck(expr)
//
//  val res = Resil().eval(expr)
//  println(res)
//
//  println()
//  println()
//
//  val blocks1 =
//    List[RslBlock](
//      SumDecl(
//        "Shape", List(), List(
//          Ctor("Square", List("side" -> IntT)),
//          Ctor("Circle", List("radius" -> IntT)),
//          Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
//        )),
//      Letrec(
//        List(
//          (CtorPattern("Rectangle", List(
//            RslVar("width"), RslVar("height")
//          )),
//            Data("Rectangle", List(I(15), I(7))))
//        ),
//        Variable("width")
//      )
//    )
//
//
//  blocks1(1) match
//    case decl: RslDecl => ()
//    case exp: RslExp => {
//      println(Resil().showExp(exp))
//    }
//    case _ => ()
//
//  Typing().typecheck(blocks1)
//
//  val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks1)
//
//  res1.tail.map { it => Resil().show(it) }.foreach(println)
//
//
//
//  println()
//  println()
//
//  val blocks2 =
//    List[RslBlock](
//      SumDecl(
//        "Shape", List(), List(
//          Ctor("Square", List("side" -> IntT)),
//          Ctor("Circle", List("radius" -> IntT)),
//          Ctor("Rectangle", List("width" -> IntT, "height" -> IntT)) // Type not checked yet
//        )),
////      Letrec(
////        List(
////          (CtorPattern("Rectangle", List(
////            RslVar("width"), RslVar("height")
////          )),
////            Data("Rectangle", List(I(15), I(7))))
////        ),
////        Variable("width")
////      )
//      Match(Data("Rectangle", List(I(15), I(7))),
//        List(
//          (CtorPattern("Square", List(RslVar("largeness"))), Components(List(S("Square I"), Variable("largeness"), I(0)), 2)),
//          (CtorPattern("Rectangle", List(RslVar("w"), RslVar("h"))), Components(List(S("Rect I"), Variable("w"), Variable("h")), 3)),
//          (WildcardPattern, Components(List(S("Other case"), I(0), I(0)), 3)),
//        )
//      ),
//      Match(Data("Circle", List(I(22))),
//        List(
//          (CtorPattern("Square", List(RslVar("largeness"))), Components(List(S("Square II"), Variable("largeness")), 2)),
//          (CtorPattern("Rectangle", List(RslVar("w"), RslVar("h"))), Components(List(S("Rect II"), Binop(Binary.ADD, Variable("w"), Variable("h"))), 2)),
//          (WildcardPattern, Components(List(S("Other case"), I(0)), 2)),
//        )
//      )
//    )
//
//
//
//  blocks2.tail.zipWithIndex.foreach { (it, i) => it match
//    case decl: RslDecl => ()
//    case exp: RslExp => {
//      println(s"[Case $i]:\n" ++  Resil().showExp(exp) ++ "\n" )
//    }
//    case _ => ()
//  }
//
//  Typing().typecheck(blocks2)
//
//  val (env2, res2) = Resil().evalBlocks(newEnvironment)(blocks2)
//
//  res2.tail.map { it => Resil().show(it) }.foreach(println)

    val blocks1 =
      List[RslBlock](
        SumDecl(
          "ListA", List(TypeVarT("A")), List(
            Ctor("Nil", List()),
            Ctor("Cons", List("value" -> TypeVarT("A"), "next" -> TypeParamT("ListA", List(TypeVarT("A")))))
          )),
        SumDecl(
          "Option", List(TypeVarT("A")), List(
            Ctor("None", List()),
            Ctor("Some", List("value" -> TypeVarT("A")))
          )),
         Func("x", Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List())))))))),
         Func("x", Data("Cons", List(
           Binop(Binary.ADD, Variable("x"), I(1)), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List()))))))))
//        Func("x", Match(Variable("x"), List(
//          (CtorPattern("Nil", List()), Data("Nil", List())),
//          (CtorPattern("Cons", List(RslVar("y"), RslVar("ys"))), Data("Cons", List(
//            Data("Cons", List(Binop(Binary.ADD, Variable("f"), Variable("x"))))
//          )))
//          ))
        )
          
          // Y-combinator?

        // Option[List[Int]] => fail
        // Type check failed, reason: Type check error with int and ListA<X8:int>::Cons
//        Data("Some", List(
//          Data("Cons", List(I(3), Data("Cons", List(I(4), Data("Cons", List(I(5), Data("Nil", List())))))))
//        ))
//        Data("Cons", List(I(3), Data("Cons", List(I(4), Data("Cons", List(I(5), Data("Nil", List()))))))),
//        Data("Nil", List())


      // Func("x", Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List()))))))))
      //
      
      // 
      // Func("x", Data("Cons", List(
      //   Binop(Binary.ADD, Variable("x"), I(1)), Data("Cons", List(Variable("x"), Data("Cons", List(Variable("x"), Data("Nil", List()))))))))


//    blocks1(1) match
//      case decl: RslDecl => ()
//      case exp: RslExp => {
//        println(Resil().showExp(exp))
//      }
//      case _ => ()
//
//    Typing().typecheck(blocks1)
//
//    val (env1, res1) = Resil().evalBlocks(newEnvironment)(blocks1)
//
//    res1.tail.map { it => Resil().show(it) }.foreach(println)

      // let f = fun x -> (x, x) in
      // let g = f 1 in
      // let h = f "hello"

      val func = Letrec(
        List(
          (RslVar("f"), Func("x", Components(List(Variable("x"), Variable("x")), 2)))
        ),
        Letrec(
          List(
            (RslVar("g"), Call(Variable("f"), I(1)))
          ),
          Variable("f")
        )
      )

      val colFuns = Letrec(
        List(
          (RslVar("head"), Func("list", Match(Variable("list"), List(
            (ListPattern(List()), Variable("list")),
            (ListPattern(List(RslVar("x"), RslVar("xs"))), Variable("x"))
          )))),
          (RslVar("tail"), Func("list", Match(Variable("list"), List(
            (ListPattern(List()), Variable("list")),
            (ListPattern(List(RslVar("x"), RslVar("xs"))), Variable("xs"))
          ))))
        ),
        Call(Variable("head"), AList(List(I(3), I(4), I(5), I(6), I(7))))
      )

      Typing().typecheck(colFuns)
      println(Resil().eval(colFuns))


//    val expr1 = Letrec(
//      List(
//        (TuplePattern(List(
//          RslVar("a"), RslVar("b"), RslTypedVar(RslVar("c"), BoolT))
//        ),
//          Components(List(I(3), I(4), I(3)), 3))
//      ),
//      Variable("c")
//    )
//
//    Typing().typecheck(expr1)
  
  
  /*  
      val map = \f => \ys => ys match
        case [] => []
        case x :: xs => f x :: map xs
   */

//  // TODO: add full workable example
//
//    val blocks2 =
//      List[RslBlock](
//        SumDecl(
//          "Shape", List(), List(
//            Ctor("Square", List(("side", IntT))),
//            Ctor("Circle", List(("radius", IntT))),
//            Ctor("Rectangle", List(("width", IntT), ("height", IntT)))
//          )),
//        Data("Rectangle", List(S("str"), I(8))) // Will raise mismatch error, but not typed currently
//      )
//
//    Typing().typecheck(blocks2)
//    val (env2, res2) = Resil().evalBlocks(newEnvironment)(blocks2)
//
//  res2.map { it => Resil().show(it) }.foreach(println)

}

def typeclassExample = {
  val tree: RslClassTree =
    AnyClass(List(
      ClassTreeNode("Functor", List("F"), List(
        ClassTreeNode("Applicative", List("A"), List(
          ClassTreeNode("Monad", List("M"), List())
        ))
      ))
    ))

  val block =
    List[RslBlock](
      SumDecl(
        "Option", List(TypeVarT("X")), List(
          Ctor("Some", List("value" -> TypeParamT("X", List()))),
          Ctor("None", List())
        )),
      SumDecl(
        "LinkedList", List(TypeVarT("X")), List(
          Ctor("Cons", List("next" -> TypeParamT("X", List()))),
          Ctor("Nil", List())
        )),
      ClassDecl(
        "Functor", List("F"),
        ClassBlock(List(
          VirtualMethodDecl("fmap",
            FuncT(
              FuncT(TypeParamT("A", List()), TypeParamT("B", List())),
              FuncT(
                TypeParamT("F", List(TypeParamT("A", List()))),
                TypeParamT("F", List(TypeParamT("B", List())))
              )
            )),
          ActualMethodDecl("map",
            Func("f", Func("x", Call(Variable("f"), Variable("x"))))
          )
        ))
      ),
      ClassDecl(
        "Applicative", List("F"),
        ClassBlock(List(
          VirtualMethodDecl("pure",
            FuncT(
              TypeParamT("A", List()),
              TypeParamT("F", List(TypeParamT("A", List()))))
          ),
          VirtualMethodDecl("apply",
            FuncT(
              TypeParamT("F", List(
                FuncT(TypeParamT("A", List()), TypeParamT("B", List())))),
              FuncT(
                TypeParamT("F", List(TypeParamT("A", List()))),
                TypeParamT("F", List(TypeParamT("B", List())))
              )
            ))
        )
        )),
      InstanceDecl("Option", Map("Functor" -> List("F"), "Applicative" -> List("F")),
        ClassBlock(List(
          ActualMethodDecl("fmap",
            Func("f", Func("xs",
              Match(Variable("xs"), List(
                (CtorPattern("None", List()), Data("None", List())),
                (CtorPattern("Some", List(RslVar("x"))), Data("Some", List(
                  Call(Variable("f"), Variable("x"))
                )))
              ))
            ))
          ),
          ActualMethodDecl("pure",
            Func("x", Data("Some", List(Variable("x"))))
          ),
          ActualMethodDecl("apply",
            Func("fs", Func("xs",
              Match(Components(List(Variable("fs"), Variable("xs")), 2), List[(RslPattern, RslExp)](
                (TuplePattern(List(RslVar("f"), RslVar("x"))),
                  Data("Some", List(Call(Variable("f"), Variable("x"))))),
                (WildcardPattern, Data("None", List()))
              ))
            ))
          ),
        ))
      ),
      Call(
        Call(Variable("apply"),
          Call(Variable("pure"),
            Func("x", Binop(Binary.ADD, Variable("x"), I(1))))),
        Data("Some", List(I(3))))
    )

}