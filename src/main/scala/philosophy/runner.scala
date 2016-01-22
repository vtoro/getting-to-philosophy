package philosophy

import language.higherKinds

import philosophy.RFuture._
import philosophy.free.UI.toInputOutput

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}

sealed trait PRGType
case object PRGFree extends PRGType
case object PRGFintag extends PRGType
case object PRGOo extends PRGType
//case object PRGEff

sealed trait OutputType
case object OConsole extends OutputType
case object OHTML extends OutputType
case object OGraph extends OutputType

sealed trait WikiType
case object WJsonp extends WikiType
case object WTest extends WikiType

sealed trait InputType
case object IRandomPage extends InputType
case class ISelectedPage(page: String ) extends InputType

@JSExport
object runner {
  var prgtype : PRGType = PRGOo
  def setprgype( p : PRGType ) = {
    println(s"PRGType: $p")
    prgtype = p
  }

  var firstpage : InputType = IRandomPage
  def setpage( p : InputType ) = {
    println(s"First page: $p")
    firstpage = p
  }

  var output : OutputType = OGraph
  def setoutput( o : OutputType ) = {
    println(s"Output type: $o")
    o match {
      case OGraph => scala.scalajs.js.Dynamic.global.setupGraph()
      case OHTML => scala.scalajs.js.Dynamic.global.showPageContent()
      case _ =>
    }
    output = o
  }

  var wikitype : WikiType = WJsonp
  def setwiki( w: WikiType ) = {
    println(s"Wiki: $w")
    wikitype = w
  }

  @JSExport
  def oo() = setprgype( PRGOo )
  @JSExport
  def free() = setprgype( PRGFree )
  @JSExport
  def fintag() = setprgype( PRGFintag )
//  @JSExport
//  def eff() = setprgype( PRGEff )
  @JSExport
  def random() = setpage( IRandomPage )
  @JSExport
  def const() = setpage( ISelectedPage( org.scalajs.dom.document.getElementById("consttext").asInstanceOf[js.Dynamic].value.asInstanceOf[String] ) )
  @JSExport
  def console() = setoutput( OConsole )
  @JSExport
  def page() = setoutput( OHTML )
  @JSExport
  def graph() = setoutput( OGraph )
  @JSExport
  def jsonpwiki() = setwiki( WJsonp )
  @JSExport
  def testwiki() = setwiki( WTest )


  @JSExport
  def run() = {
    val contentdiv = org.scalajs.dom.document.getElementById("content")
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    prgtype match {
      case PRGOo =>
        import philosophy.oop._
        val wiki : Wiki = wikitype match {
          case WJsonp => JsonpWiki
          case WTest => TestWiki
        }
        val ui : UI = new UIImpl(
          output match {
            case OConsole => new ConsoleOutput()
            case OHTML => new HTMLOutput( contentdiv )
            case OGraph => GraphOutput
          },
          firstpage match {
            case IRandomPage => new RandomPageInput( wiki )
            case ISelectedPage(x) => new ConstInput( x )
          }
        )
        program.run(wiki,ui).onComplete {
          case Success( s ) => println(s"Done! $s")
          case Failure( e ) => println(s"Failed: $e")
        }
      case PRGFintag =>
        import philosophy.finallytagless._
        val wikiintp = wikitype match {
          case WJsonp => Interpreter( JsonpWiki )
          case WTest => Interpreter( TestWiki ) andThen IoToRFuture
        }
        val inputintp = firstpage match {
          case IRandomPage => RandomPageInput( wikiintp )
          case ISelectedPage( x ) => Interpreter( new ConstInput(x) ) andThen IdToRFuture
        }
        val outputintp =
          output match {
            case OConsole => Interpreter( ConsoleOutput ) andThen IoToRFuture
            case OHTML => Interpreter( new HTMLOutput(contentdiv) ) andThen IoToRFuture
            case OGraph => Interpreter( GraphOutput ) andThen IoToRFuture
          }

        val inputAndOutput = inputintp and outputintp
        val uiintp = UIToInputOutput( inputAndOutput )
        val wikiAndUi = wikiintp and uiintp

        program
          .run
          .run( wikiAndUi )
          .apply( ec )
          .onComplete {
            case Success( s ) => println(s"Done! $s")
            case Failure( e ) => println(s"Failed: $e")
          }
      case PRGFree =>
        import philosophy.free._
        val wikiintp = wikitype match {
          case WJsonp => Wiki.JsonpInterpreter
          case WTest => Wiki.TestInterpreter andThen IoToRFuture
        }
        val inputintp =
          firstpage match {
            case IRandomPage => Input.RandomPageInterpreter andThen wikiintp
            case ISelectedPage(x) => new Input.ConstInterpreter(x) andThen IdToRFuture
          }
        val outputintp =
          output match {
            case OConsole => Output.ConsoleInterpreter andThen IoToRFuture
            case OHTML => new Output.HTMLInterpreter( contentdiv ) andThen IoToRFuture
            case OGraph => Output.GraphInterpreter andThen IoToRFuture
          }
        val uiintp = new toInputOutput[RFuture](inputintp,outputintp)
        val prgintp = uiintp or wikiintp

        program
          .run
          .foldMap( prgintp )
          .apply( ec )
          .onComplete {
            case Success( s ) => println(s"Done! $s")
            case Failure( e ) => println(s"Failed: $e")
          }
    }
  }

}
