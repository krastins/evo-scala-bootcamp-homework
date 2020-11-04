package async

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success}

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 *
 * Usage: sbt "runMain async.AsyncHomework https://google.com/"
 */
object AsyncHomework extends App {
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  val url: Option[String] = args.headOption

  val serverNames = url match {
    case Some(value) => for {
      body <- fetchPageBody(value)
      urls <- findLinkUrls(body)
      servers <- Future.traverse(urls)(fetchServerName)
    } yield servers.flatten.distinct.sortBy(_.toLowerCase)
    case None => Future.failed(new IllegalArgumentException("Missing argument: URL"))
  }

  serverNames.onComplete(result => {
    result match {
      case Success(names: List[String]) => println(names.mkString(", "))
      case Failure(e) => println(e.getMessage)
    }
    ec.shutdown() // don't wait for the CachedThreadPool keepAliveTime (60s) to exit
  })

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      }
      finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
