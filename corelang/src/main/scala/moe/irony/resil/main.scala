package moe.irony.resil

import moe.irony.resil.sig.{RslExp, RslVar}

import scala.collection.immutable.List

def mkEnv(bindings: (String, RslExp)*): List[(RslVar, RslExp)] = {
  bindings.map{ (s, e) =>  (RslVar(s), e) }.toList
}






@main
def main(): Unit = {
  Examples.evalSamples()
}

