package lang_visualizer

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import com.raquo.laminar.api.L.{*, given}
import moe.irony.resil.lang.Resil
import moe.irony.resil.lexer.{Parser, Tokenizer}
import moe.irony.resil.sig.{I, BoolV, ClosV, ErrV, IntV, PairV, PromV, RslVal, StrV, UnitV}

import scala.util.Random
import org.scalajs.dom
import typings.chartJs.mod.*

// import javascriptLogo from "/javascript.svg"
@js.native @JSImport("/javascript.svg", JSImport.Default)
val javascriptLogo: String = js.native

@main
def evaluator(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    EvalMain.appElement()
  )

final class EvalResult

case class EvalResultValue(var value: String)



def execute(code: String): RslVal =
//  val tokenizer = Tokenizer
//  val tokens = tokenizer.tokenize(code)
//  val ast = Parser().parse(tokens)
//  val res = Resil().eval(ast)
  val i = I(code.toInt)
  val res = Resil().eval(i)
  res
  

final class ResilModel:
  val dataVar: Var[EvalResultValue] =
    Var(EvalResultValue(""))
  val dataSignal: StrictSignal[EvalResultValue] = dataVar.signal

//  def evaluate(code: String): Unit =
//    dataVar.update(_ => EvalResultValue(execute(code)))



object EvalMain:
  val model = new ResilModel
  import model.*

  def appElement(): Element =
    div(
      h1("Evaluator"),
      renderInputArea(),
      renderOutputArea()
    )

  def renderInputArea(): Element =
    div(
      renderInputBox(dataSignal)
    )

  def renderInputBox(itemSignal: Signal[EvalResultValue]): Element =
    div(
      inputForTextArea(
        itemSignal.map(_.value),
        makeDataItemUpdater({ (data, newValue) =>
          data.copy(value = newValue)
        })
      )
    )

  def makeDataItemUpdater[A](f: (EvalResultValue, A) => EvalResultValue): Observer[A] =
    dataVar.updater { (data, newValue) =>
        f(data, newValue)
    }

  def inputForTextArea(valueSignal: Signal[String], valueUpdater: Observer[String]): TextArea =
    textArea(
      value <-- valueSignal,
      onInput.mapToValue --> valueUpdater,
    )


  def renderOutputArea(): Element =
    div(
      child.text <-- dataSignal.map(code =>
        val res = execute(code.value)
        Resil().show(res)
      )
    )






