package example

import cats.syntax.all._
import cats.effect._
import cats.effect.std.Console
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s._
import org.http4s.client.Client
import org.http4s.syntax.all._
import org.http4s.circe._
import org.http4s.dsl.request._
import io.circe._
// import epollcat.EpollApp
import fs2.io.net.Network
// import fs2.io.net.tls.S2nConfig
import fs2.io.net.tls.TLSContext
import com.comcast.ip4s._
import com.google.firestore.v1.firestore.Firestore
import com.google.firestore.v1.firestore.CreateDocumentRequest
import com.google.firestore.v1.document.Document
import com.google.firestore.v1.firestore.ListDocumentsRequest
import com.google.firestore.v1.document.Value
import com.google.firestore.v1.document.Value.ValueTypeOneof
import com.google.firestore.v1.write.Write
import com.google.firestore.v1.write.Write.Operation.Update
import org.http4s.server.middleware.ErrorAction
import org.http4s.server.middleware.ErrorHandling
import cats.data.Kleisli
import com.google.firestore.v1.firestore.ListCollectionIdsRequest
import com.google.firestore.v1.firestore.GetDocumentRequest
import com.google.firestore.v1.firestore.GetDocumentRequest.ConsistencySelector

object EmberExample extends IOApp {

  final case class Joke(joke: String)
  object Joke {
    implicit val jokeDecoder: Decoder[Joke] = Decoder.derived[Joke]
    implicit def jokeEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, Joke] =
      jsonOf
  }

  def run(args: List[String]): IO[ExitCode] =
    val clients = for {
      // tls <- customTLS
      tlsClient <- createClient()
    } yield tlsClient

    clients
      .use { tlsClient =>
        createServer(tlsClient, createFirestoreClient(tlsClient)).useForever
      }
      .as(ExitCode.Success)

  def createFirestoreClient(client: Client[IO]): Firestore[IO] =
    Firestore.fromClient[IO](
      client,
      Uri
        .fromString("https://firestore.googleapis.com")
        .getOrElse(throw new RuntimeException("invalid firestore uri"))
    )

  def withErrorHandling(routes: Kleisli[IO, Request[IO], Response[IO]]) = {
    ErrorHandling.Recover.total(
      ErrorAction.log(
        routes,
        messageFailureLogAction = (t, msg) =>
          IO.println(t) >>
            IO.println(t.getStackTrace().map(_.toString()).mkString("\n")),
        serviceErrorLogAction = (t, msg) =>
          IO.println(t) >>
            IO.println(t.getStackTrace().map(_.toString()).mkString("\n")),
      )
    )
  }

  def createServer(
      client: Client[IO],
      firestore: Firestore[IO]
  ): Resource[IO, Unit] = {
    EmberServerBuilder
      .default[IO]
      .withHttp2
      .withHttpApp(withErrorHandling(app(client, firestore).orNotFound))
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .build
      .void
  }

  def app(client: Client[IO], firestore: Firestore[IO]) = HttpRoutes.of[IO] {
    case GET -> Root =>
      Response[IO](Status.Ok).withEntity("Hey There!").pure[IO]
    case GET -> Root / "hello" / person =>
      Response[IO](Status.Ok).withEntity(s"Hello, $person").pure[IO]
    case GET -> Root / "joke" =>
      getJoke(client).map { joke =>
        Response(Status.Ok).withEntity(joke)
      }
    case GET -> Root / "savejoke" =>
      val docId = _root_.java.util.UUID.randomUUID().toString()
      val parent = "projects/tanishiking-dev/databases/ember/documents"
      val joke = "I'm a joke"
      val doc = Document.of(
        name = createDocumentName("tanishiking-dev", "ember", "jokes", docId),
        fields = Map("joke" -> Value.of(ValueTypeOneof.StringValue(joke))),
        createTime = None,
        updateTime = None
      )
      val req =
        CreateDocumentRequest.of(parent, "jokes", docId, Some(doc), None)
      println(req)
      firestore.getDocument(
        GetDocumentRequest.of(
          name = createDocumentName("tanishiking-dev", "ember", "jokes", docId),
          mask = None,
          consistencySelector = ConsistencySelector.Empty,
        ),
        Headers.empty
      ).flatMap { doc =>
        println("aaa")
        firestore.createDocument(req, Headers.empty).map { _ =>
          Response(Status.Ok).withEntity(joke)
        }
      }
    }

  def getJoke(client: Client[IO]): IO[String] =
    client
      .expect[Joke](Request(Method.GET, uri"https://icanhazdadjoke.com/"))
      .map(_.joke)

  def createClient(): Resource[IO, Client[IO]] = {
    EmberClientBuilder
      .default[IO]
      .withHttp2
      .build
  }

  // TLS 1.3 is not supported without a different default
  // def customTLS =
  //   S2nConfig.builder
  //     .withCipherPreferences("default_tls13")
  //     .build[IO]
  //     .map(Network[IO].tlsContext.fromS2nConfig(_))

  private def createDocumentName(
      projectId: String,
      databaseId: String,
      collectionId: String,
      jokeId: String
  ): String = {
    val documentPath = s"projects/$projectId/databases/$databaseId/documents"
    s"$documentPath/$collectionId/$jokeId"
  }
}


