package philosophy.oop

import scala.concurrent.{ExecutionContext, Future}
import org.scalajs.dom.Element
import philosophy.{Graph, wikiapi}
import philosophy.crawlStates._

trait Wiki {
  def randomPage( implicit ec: ExecutionContext ) : Future[String]
  def nextLinks( current:String )(implicit ec:ExecutionContext ) : Future[List[String]]
}

object JsonpWiki extends Wiki {
  def randomPage(implicit ec: ExecutionContext): Future[String] = wikiapi.randomPage
  def nextLinks(current: String)(implicit ec: ExecutionContext): Future[List[String]] = wikiapi.nextLinks( current )
}

object TestWiki extends Wiki {
  def randomPage(implicit ec: ExecutionContext): Future[String] = Future { wikiapi.testrandom }
  def nextLinks(current: String)(implicit ec: ExecutionContext): Future[List[String]] =
    Future { wikiapi.testnextlink(current)}
}

trait Output {
  def firstPage( name:String ) : Unit
  def pageStep( from:String, to:String, idx: Int ) : Unit
  def statusMsg(msg:String ) : Unit
}

class ConsoleOutput extends Output {
  def firstPage(name:String ) : Unit = println( s"1: $name")
  def pageStep( from:String, to:String, idx : Int ) : Unit = println(s"$idx: $to")
  def statusMsg(msg:String ): Unit = println(msg)
}

class HTMLOutput(e: Element ) extends Output {
  def firstPage( name:String ) : Unit = {
    e.innerHTML = s"<p>1: <a href='http://en.wikipedia.org/wiki/$name'>$name</a></p>\n"
  }
  def pageStep( from:String, to:String, idx: Int ) : Unit = {
    e.innerHTML += s"<p>$idx: <a href='http://en.wikipedia.org/wiki/$to'>$to</a></p>\n"
  }
  def statusMsg(msg:String ): Unit = {
    e.innerHTML += s"<p>$msg</p>\n"
  }
}

object GraphOutput extends Output {
  def firstPage(name: String): Unit = Graph.addNode( name )
  def pageStep(from: String, to: String, idx: Int): Unit = {
    Graph.addNode( to )
    Graph.addLink( from, to )
  }
  def statusMsg(msg: String): Unit = {
    println( msg )
  }
}

trait Input {
  def getPage(implicit ec: ExecutionContext ) : Future[String]
}

class RandomPageInput( wiki: Wiki ) extends Input {
  def getPage(implicit ec: ExecutionContext ) : Future[String] = wiki.randomPage
}

class ConstInput( string: String ) extends Input {
  def getPage(implicit ec: ExecutionContext ) : Future[String] = Future.successful( string )
}

trait UI {
  def getStartPage(implicit ec:ExecutionContext) : Future[Continue]
  def stepPage( state: Continue ) : Unit
  def finished( state: CrawlStateFinished ) : Unit
}


class UIImpl(output:Output, uiInput: Input ) extends UI {
  def getStartPage(implicit ec:ExecutionContext): Future[Continue] =
    uiInput.getPage.map{ first =>
      output.firstPage(first)
      Continue( first, Nil )
    }

  def stepPage( state: Continue ) : Unit = output.pageStep( state.visitedPages.head, state.currentPage, state.visitedPages.length+1 )

  def finished( state: CrawlStateFinished ) : Unit = output.statusMsg(
    state match {
      case Loop( currentPage, to ) => s"Page '$currentPage' loops back to $to"
      case Error( currentPage, e ) => s"Error '$e' at Page '$currentPage'"
      case NoLinks( currentPage ) => s"No suitable links from Page '$currentPage'"
      case AtPhilosophy( steps ) => s"Got to Philosophy in $steps steps!"
    }
  )
}

object program {

  def stepNext( wiki: Wiki, ui: UI, state: Continue )( implicit ec: ExecutionContext ) : Future[CrawlState] =
    wiki
      .nextLinks( state.currentPage)
      .map(
        _.headOption.fold
          ( NoLinks(state.currentPage) : CrawlState )
          { nextPage =>
            val cont = Continue(nextPage, state.currentPage :: state.visitedPages)
            ui.stepPage( cont )
            if (state.visitedPages.contains(nextPage))
              Loop(state.currentPage, nextPage)
            else if (nextPage.toLowerCase == "philosophy")
              AtPhilosophy(state.visitedPages.length + 2)
            else
              cont
          }
      )
      .recover{ case e => Error(state.currentPage, e.toString ) }

  def recurseStep( wiki : Wiki, ui : UI, state: Continue )( implicit ec: ExecutionContext ) : Future[CrawlStateFinished] =
    stepNext( wiki, ui, state )
      .flatMap {
        case c : Continue => recurseStep( wiki, ui, c )
        case f : CrawlStateFinished => Future.successful( f )
      }

  def run( wiki : Wiki, ui: UI )( implicit ec: ExecutionContext ) : Future[CrawlStateFinished] =
    ui.getStartPage
      .flatMap( recurseStep( wiki, ui, _ ) )
      .map{ state => ui.finished(state); state }
}