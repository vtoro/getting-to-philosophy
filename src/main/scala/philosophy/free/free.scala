package philosophy.free


import language.higherKinds
import scala.scalajs.js.JSApp
import org.scalajs.dom.{Element}
import cats.data.{Xor, Coproduct}
import cats.free.Free
import cats.{Id, Monad, ~>}
import philosophy.{Graph, wikiapi, RFuture}
import philosophy.RFuture._
import philosophy.IO._
import philosophy.crawlStates._

sealed trait Wiki[A]
object Wiki {
  case object RandomPage extends Wiki[String]
  case class NextLinks(current:String ) extends Wiki[List[String]]

  object JsonpInterpreter extends (Wiki ~> RFuture) {
    def apply[A](fa: Wiki[A]): RFuture[A] = { implicit ec =>
      fa match {
        case RandomPage => wikiapi.randomPage
        case NextLinks(current) => wikiapi.nextLinks(current)
      }
    }
  }

  object TestInterpreter extends (Wiki ~> IO) {
    override def apply[A](fa: Wiki[A]): IO[A] = fa match {
      case RandomPage => Io{ wikiapi.testrandom }
      case NextLinks( current ) => Io { wikiapi.testnextlink( current ) }
    }
  }
}

sealed trait Output[A]
object Output {
  case class FirstPage( name: String ) extends Output[Unit]
  case class PageStep( from:String, to:String, idx : Int ) extends Output[Unit]
  case class StatusMsg( msg:String ) extends Output[Unit]

  object ConsoleInterpreter extends (Output ~> IO) {
    def apply[A](fa: Output[A]): IO[A] =
      fa match {
        case FirstPage( name ) => Io{ println( s"1: $name") }
        case PageStep( from, to, idx ) => Io{ println( s"$idx: $to")}
        case StatusMsg( msg ) => Io{ println( msg ) }
      }
  }

  class HTMLInterpreter(e:Element ) extends (Output ~> IO) {
    def apply[A](fa: Output[A]): IO[A] =
      fa match {
        case FirstPage( name ) => Io{
          e.innerHTML = s"<p>1: <a href='http://en.wikipedia.org/wiki/$name'>$name</a></p>\n"
        }
        case PageStep( from, to, idx ) => Io{
          e.innerHTML += s"<p>$idx: <a href='http://en.wikipedia.org/wiki/$to'>$to</a></p>\n"
        }
        case StatusMsg( msg ) => Io{
          e.innerHTML += s"<p>$msg</p>\n"
        }
      }
  }

  object GraphInterpreter extends (Output ~> IO) {
    override def apply[A](fa: Output[A]): IO[A] = fa match {
      case FirstPage( name ) => Io { Graph.addNode( name )}
      case PageStep( from, to, idx ) => Io { Graph.addNode( to ); Graph.addLink( from, to ) }
      case StatusMsg( msg ) => Io { println( msg ) }
    }
  }
}


sealed trait Input[A]
object Input {
  case object GetPage extends Input[String]

  object RandomPageInterpreter extends (Input ~> Wiki) {
    def apply[A](fa: Input[A]): Wiki[A] = fa match {
      case GetPage => Wiki.RandomPage
    }
  }

  class ConstInterpreter(const: String) extends (Input ~> Id) {
    override def apply[A](fa: Input[A]): Id[A] =
      fa match {
        case GetPage => const
      }
  }
}

sealed trait UI[A]


object UI {
  case object GetStartPage extends UI[Continue]
  case class StepPage( state: Continue ) extends UI[Unit]
  case class Finished( state: CrawlStateFinished ) extends UI[Unit]

  type CIO[A] = Coproduct[Output,Input,A]
  def lright[A]( i: Input[A] ) : Free[CIO,A] = Free.liftF[CIO,A]( Coproduct.rightc[Output,Input,A]( i ) )
  def lleft[A]( o: Output[A] ) : Free[CIO,A] = Free.liftF[CIO,A]( Coproduct.leftc[Output,Input,A]( o ) )

  class toInputOutput[B[_] : Monad](i: Input ~> B, o : Output ~> B ) extends (UI ~> B){
    def apply[A](fa: UI[A]): B[A] = ((fa match {
      case GetStartPage =>
        for {
          page <- lright( Input.GetPage )
          _ <- lleft( Output.FirstPage(page) )
        } yield Continue(page, Nil)
      case StepPage(Continue(currentPage, visitedPages)) => lleft( Output.PageStep(visitedPages.head, currentPage, visitedPages.length+1))
      case Finished(state) =>
        lleft(
          Output.StatusMsg(state match {
            case Loop(currentPage, to) => s"Page '$currentPage' loops back to $to"
            case Error(currentPage, e) => s"Error '$e' at Page '$currentPage'"
            case NoLinks(currentPage) => s"No suitable links from Page '$currentPage'"
            case AtPhilosophy(steps) => s"Got to Philosophy in $steps steps!"
          })
        )
    } ): Free[CIO,A] ).foldMap( o or i )
  }

}

object program {
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  type PAlg[X] = Coproduct[UI,Wiki,X]
  type FreeP[X] = Free[PAlg,X]
  def liftP[A]( x: UI[A] ) = Free.liftF[PAlg,A]( Coproduct.leftc[UI,Wiki,A]( x ) )
  def liftP[A]( x: Wiki[A] ) = Free.liftF[PAlg,A]( Coproduct.rightc[UI,Wiki,A]( x ) )
  def pureP[A]( x : A ) = Free.pure[PAlg,A]( x )

  def stepNext( state: Continue ) : FreeP[CrawlState] =
    for {
      next <- liftP( Wiki.NextLinks( state.currentPage ) )
      finalState <-
        next.headOption
          .fold( pureP( NoLinks(state.currentPage) ) : FreeP[CrawlState] ) { nextPage =>
            val cont = Continue(nextPage, state.currentPage :: state.visitedPages)
            val newStatus =
              if (state.visitedPages.contains(nextPage)) Loop(state.currentPage, nextPage)
              else if (nextPage.toLowerCase == "philosophy") AtPhilosophy(state.visitedPages.length + 2)
              else cont
            liftP( UI.StepPage(cont) ) >> pureP( newStatus )
          }
    } yield finalState

  def recurseStep( state: Continue ) : FreeP[CrawlStateFinished] =
    stepNext( state )
      .flatMap {
        case c : Continue => recurseStep( c )
        case f : CrawlStateFinished => pureP( f )
      }

  val run: FreeP[CrawlStateFinished] =
    liftP( UI.GetStartPage )
      .flatMap( recurseStep )
      .flatMap( s => liftP( UI.Finished( s ) ) >> pureP(s) )

}