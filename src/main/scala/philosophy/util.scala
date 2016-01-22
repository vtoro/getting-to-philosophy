package philosophy

import java.util.UUID
import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.scalajs.js
import scala.util.{Failure, Success, Random, Try}
import scala.util.matching.Regex
import org.scalajs.dom
import scala.scalajs.js.Dynamic._
import scala.scalajs.js.URIUtils
import cats.{~>, Id, Monad}
import philosophy.IO.IO


object jsonp {
  def uriEncode( param: (String,String) ) : String =
    s"${URIUtils.encodeURI(param._1)}=${URIUtils.encodeURI(param._2)}"

  def uriEncode( baseUri : String, params : (String,String)* ) : String = params match {
    case Seq() => baseUri
    case x => s"$baseUri?${ params.map(uriEncode).mkString("&") }"
  }

  def apply[X](uri : String => String )( cb: js.Dynamic => X ) : Future[X] = {
    val name = "jsonp_fun_"+UUID.randomUUID().toString.replace('-','_')
    val promise = Promise.apply[X]()
    global.updateDynamic(name){ x: js.Dynamic =>
      promise.tryComplete( Try { cb(x) } )
    }
    val scriptElem = dom.document.createElement("script")
    scriptElem.setAttribute( "src", uri(name) )
    dom.document.body.appendChild( scriptElem )
    promise.future
  }
}

object wikiapi {

  private def contentUrl(page : String )( implicit ec: ExecutionContext ) : Future[String] =
    jsonp(callbackname =>
      jsonp.uriEncode(
        "https://en.wikipedia.org/w/api.php",
        "action" -> "query",
        "titles" -> page,
        "prop" -> "revisions",
        "rvprop" -> "content",
        "redirects" -> "true",
        "format" -> "json",
        "callback" -> callbackname
      )
    )( x =>
      x
        .query
        .pages
        .asInstanceOf[js.Dictionary[js.Dynamic]]
        .head
        ._2
        .revisions
        .asInstanceOf[js.Array[js.Dynamic]](0)
        .selectDynamic("*")
        .asInstanceOf[String]
    ).recoverWith {
      case e => Future.failed( new Exception(s"Could not get content of page: $page from wiki -> $e"))
    }

  def randomPage( implicit ec: ExecutionContext ) : Future[String] =
    jsonp( callbackname =>
      jsonp.uriEncode(
        "https://en.wikipedia.org/w/api.php",
        "action" -> "query",
        "list" -> "random",
        "format" -> "json",
        "rnnamespace" -> "0",
        "callback" -> callbackname
      )
    )( x =>
      x
        .query
        .random
        .asInstanceOf[js.Array[js.Dynamic]](0)
        .title
        .asInstanceOf[String]
    ).recoverWith {
      case e => Future.failed( new Exception(s"Could not get randomPage from wiki-> $e"))
    }

  //This is almost certainly wrong and incomplete
  private val curlies = """\{\{[^\{\}]*?\}\}""".r // {{ stuff }}
  private val curlypipes = """\{\|[^\{\}]*?\|\}""".r // {| more stuff |}
  private val twolevellinks = """\[\[([^\[\]]*\[\[[^\[\]]*\]\][^\[\]]*)*\]\]""".r // [[File:foo|bar|boo [[another|link]] more text]]
  private val findlinks = """\[\[(.*?)\]\]""".r // [[..linkdata..]]
  private val link1 = """^([a-z][^\:\#\|]*(?:#[^\:\|]*)?)$""".r // lowercaselink
  private val link2 = """^([^\:\#\|]*?)(?:#[^\:\|]*)?\|[a-z][^\]\|]*$""".r // link|lowercasetitle
  def remove( r: Regex ) : String => String = (s) => r.replaceAllIn(s,"")
  def rec( r: String => String, recurse : Int = 20 ) : String => String = if( recurse <= 1 ) r else r andThen rec( r, recurse - 1 )
  val clean : String => String = rec( remove( curlies ) andThen remove( curlypipes) ) andThen remove( twolevellinks )
  private def getLinks(content : String ) : List[String] = {
    findlinks
      .findAllMatchIn( clean( content ) )
      .flatMap( _.subgroups.find(_ != null).toList )
      .flatMap( l =>
        link1.findFirstMatchIn(l).flatMap(_.subgroups.find(_ != null)).toList ++
        link2.findFirstMatchIn(l).flatMap(_.subgroups.find(_ != null)).toList
      )
      .toList
  }

  def nextLinks(current:String )(implicit ec:ExecutionContext ) : Future[List[String]] =
    contentUrl( current )
      .map( getLinks )


  def testrandom : String = (Random.nextInt( 50 )+1).toString
  def testnextlink( current : String ) : List[String] =
    (Try {
      current.toInt
    } match {
      case Success( 1 ) => "Philosophy"
      case Success( n ) if n % 2 == 0 => n / 2
      case Success( n ) if n % 2 != 0 => n * 3 + 1
      case Failure(_) => current.length
    }).toString :: Nil

}


object RFuture {
  type RFuture[A] = ExecutionContext => Future[A]
  def apply[A]( a: =>A ): RFuture[A] = { implicit ec : ExecutionContext => Future { a } }
  implicit object RFutureMonad extends Monad[RFuture] {
    def flatMap[A, B](fa: RFuture[A])(f: (A) => RFuture[B]): RFuture[B] =
    { implicit ec => fa(ec).flatMap( f andThen { x => x.apply(ec) } ) }
    def pure[A](x: A): RFuture[A] =
    { implicit ec => Future { x } }
  }
  object IdToRFuture extends (Id ~> RFuture ) {
    override def apply[A](fa: Id[A]): RFuture[A] = RFuture{fa}
  }
  object IoToRFuture extends (IO.IO ~> RFuture) {
    override def apply[A](fa: IO[A]): RFuture[A] = RFuture{fa.apply()}
  }

}

object IO {
  type IO[A] = () => A
  def Io[A]( x: => A) : IO[A] = () => x
  implicit object IOMonad extends Monad[IO] {
    def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = () => f( fa.apply() ).apply()
    def pure[A](x: A): IO[A] = () => x
  }
  object IdToIO extends (Id ~> IO) {
    override def apply[A](fa: Id[A]): IO[A] = Io{fa}
  }
}


object crawlStates {
  sealed trait CrawlState
  sealed trait CrawlStateFinished extends CrawlState
  case class Continue(currentPage: String, visitedPages: List[String]) extends CrawlState
  case class Loop(currentPage: String, to: String) extends CrawlStateFinished
  case class Error(currentPage: String, e: String) extends CrawlStateFinished
  case class NoLinks(currentPage: String) extends CrawlStateFinished
  case class AtPhilosophy(steps: Int) extends CrawlStateFinished
}

object Graph {
  private def addToGraph( addition : String ) = {
    val currentGraph = scala.scalajs.js.Dynamic.global.inputGraph.asInstanceOf[String]
    if( !currentGraph.contains( addition )) {
      scala.scalajs.js.Dynamic.global.inputGraph = currentGraph + addition
      scala.scalajs.js.Dynamic.global.tryDrawGraph()
    }
  }
  def addNode( a : String ) : Unit = addToGraph( s""""$a" [labelType="html" label="<a href='http://en.wikipedia.org/wiki/$a'>$a</a>"];\n""" )
  def addLink( a:String, b:String ) : Unit = addToGraph( s""""$a" -> "$b";\n""" )
}
