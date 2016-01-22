package philosophy.finallytagless

import language.higherKinds
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import cats.{~>, Id, Monad}
import cats.syntax.flatMap._
import cats.syntax.functor._
import philosophy.{Graph, wikiapi, RFuture}
import philosophy.RFuture._
import philosophy.IO._
import philosophy.crawlStates._

trait Wiki[F[_]] {
  def randomPage : F[String]
  def nextLinks(current: String ) : F[List[String]]
}

object Wiki {
  def randomPage : Term[Wiki,String] = Term[Wiki] { _.randomPage }
  def nextLinks(current: String ) : Term[Wiki,List[String]] = Term[Wiki] { _.nextLinks(current) }
}

object JsonpWiki extends Wiki[RFuture] {
  def randomPage: RFuture[String] = (ec) => wikiapi.randomPage(ec)
  def nextLinks(current: String): RFuture[List[String]] = (ec) => wikiapi.nextLinks(current)(ec)
}

object TestWiki extends Wiki[IO] {
  def randomPage: IO[String] = Io{ wikiapi.testrandom }
  def nextLinks(current: String): IO[List[String]] = Io{ wikiapi.testnextlink(current) }
}

trait Output[F[_]] {
  def firstPage( name: String ) : F[Unit]
  def pageStep( from:String, to:String, idx : Int ) : F[Unit]
  def statusMsg( msg:String ) : F[Unit]
}

object ConsoleOutput extends Output[IO] {
  def firstPage(name: String): IO[Unit] = Io{ println( s"1: $name") }
  def pageStep(from: String, to: String, idx: Int): IO[Unit] = Io{ println(s"$idx: $to") }
  def statusMsg(msg: String): IO[Unit] = Io{ println(msg) }
}

class HTMLOutput( outputElement: Element ) extends Output[IO] {
  def firstPage( name:String ) : IO[Unit] = Io {
    outputElement.innerHTML = s"<p>1: <a href='http://en.wikipedia.org/wiki/$name'>$name</a></p>\n"
  }
  def pageStep( from:String, to:String, idx: Int ) : IO[Unit] = Io {
    outputElement.innerHTML += s"<p>$idx: <a href='http://en.wikipedia.org/wiki/$to'>$to</a></p>\n"
  }
  def statusMsg(msg:String ): IO[Unit] = Io {
    outputElement.innerHTML += s"<p>$msg</p>\n"
  }
}

object GraphOutput extends Output[IO] {
  override def firstPage(name: String): IO[Unit] = Io { Graph.addNode( name ) }
  override def pageStep(from: String, to: String, idx: Int): IO[Unit] = Io {
    Graph.addNode( to )
    Graph.addLink( from, to )
  }
  override def statusMsg(msg: String): IO[Unit] = Io {
    println( msg )
  }
}

trait Input[F[_]] {
  def getPage : F[String]
}

object RandomPageInput extends ( Input ~~> Wiki ) {
  override def embed[M[_] : Monad]( wiki: Interpreter[Wiki, M]): Input[M] = new Input[M] {
    override def getPage: M[String] = wiki( _.randomPage )
  }
}

class ConstInput( input:String ) extends Input[Id] {
  def getPage : String = input
}

trait UI[F[_]] {
  def getStartPage : F[Continue]
  def stepPage( state: Continue ) : F[Unit]
  def finished( state: CrawlStateFinished ) : F[Unit]
}

object UI {
  def getStartPage : Term[UI,Continue] = Term[UI]{ _.getStartPage }
  def showStep(state: Continue ) : Term[UI,Unit] = Term[UI]{ _.stepPage(state) }
  def finished(state: CrawlStateFinished ) : Term[UI,Unit] = Term[UI]{ _.finished(state) }
}

object UIToInputOutput extends ( UI ~~> IPair[Input,Output,?[_]] ) {
  def embed[M[_] : Monad]( inputOutput: Interpreter[IPair[Input,Output,?[_]], M]): UI[M] = new UI[M] {
    val (input,output) = Interpreter.pairOf( inputOutput )
    def getStartPage: M[Continue] = for {
      page <- input {_.getPage}
      _ <- output {_.firstPage(page)}
    } yield Continue(page, Nil)

    def stepPage(state: Continue): M[Unit] = output {_.pageStep(state.visitedPages.head, state.currentPage, state.visitedPages.length + 1)}

    def finished(state: CrawlStateFinished): M[Unit] =
      output{_.statusMsg(state match {
        case Loop(currentPage, to) => s"Page '$currentPage' loops back to $to"
        case Error(currentPage, e) => s"Error '$e' at Page '$currentPage'"
        case NoLinks(currentPage) => s"No suitable links from Page '$currentPage'"
        case AtPhilosophy(steps) => s"Got to Philosophy in $steps steps!"
      })}
  }
}


object program {
  type WikiAndUI[M[_]] = IPair[Wiki,UI,M]
  type PRG[X] = Term[WikiAndUI,X]
  def pure[X]( x:X ) : PRG[X] = Term.pure[WikiAndUI,X]( x )

  def stepNext( state : Continue ) : PRG[CrawlState] =
    for {
      links <- Wiki.nextLinks( state.currentPage ).embed[WikiAndUI]
      nextState <- links.headOption.fold( pure( NoLinks(state.currentPage) : CrawlState ) ) {
        nextPage =>
          val cont = Continue(nextPage, state.currentPage :: state.visitedPages)
          UI.showStep( cont ).embed[WikiAndUI].map{ ignore =>
            if (state.visitedPages.contains(nextPage))
              Loop(state.currentPage, nextPage)
            else if (nextPage.toLowerCase == "philosophy")
              AtPhilosophy(state.visitedPages.length + 2)
            else
              cont
          }
      }
    } yield nextState

  def recurseStep( state: Continue ) : PRG[CrawlStateFinished] =
    stepNext( state )
      .flatMap {
        case c : Continue => recurseStep( c )
        case f : CrawlStateFinished => pure( f )
      }

  def run : PRG[CrawlStateFinished] =
    UI.getStartPage.embed[WikiAndUI]
      .flatMap( recurseStep )
      .flatMap{ state => UI.finished(state).map( x => state ).embed[WikiAndUI] }

}
