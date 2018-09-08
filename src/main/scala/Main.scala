import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.Done
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

import scala.io.StdIn
import scala.concurrent.Future

object WebServer {

  // needed to run the route
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // needed for the future map/flatmap in the end and future in fetchItem and saveOrder
  implicit val executionContext = system.dispatcher

  var orders: List[Item] = Nil

  // domain model
  //final case class Item(name: String, id: Long)
  //final case class Order(items: List[Item])

  case class Student(name: String, id: String, notes: List[Note], courses: List[Course])
  case class Note(value: Double, percentage: Int)
  case class Course(students: List[Student], name: String, id: String)


  // formats for unmarshalling and marshalling
  //implicit val itemFormat = jsonFormat2(Item)
  //implicit val orderFormat = jsonFormat1(Order)

  implicit val studentFormat = jsonFormat4(Student)
  implicit val noteFormat = jsonFormat2(Note)
  implicit val courseFormat = jsonFormat3(Course)

  // (fake) async database query api

  object Operation {


    /*def fetchItem(itemId: Long): Future[Option[Item]] = Future {
      orders.find(o => o.id == itemId)
    }

    def fetchAll(): Future[List[Item]] = Future {
      orders
    }

    def saveOrder(order: Order): Future[Done] = {
      orders = order match {
        case Order(items) => items ::: orders
        case _ => orders
      }
      Future {
        Done
      }
    }*/
  }



  def main(args: Array[String]) {

    val route: Route =
      get {
        pathPrefix("item" / LongNumber) { id =>
          // there might be no item for a given id
          val maybeItem: Future[Option[Item]] = Operation.fetchItem(id)

          onSuccess(maybeItem) {
            case Some(item) => complete(item)
            case None       => complete(StatusCodes.NotFound)
          }
        }
      } ~
      post {
        path("create-order") {
          entity(as[Order]) { order =>
            val saved: Future[Done] = Operation.saveOrder(order)
            onComplete(saved) { done =>
              complete("order created")
            }
          }
        }
      }~
      get {
        pathPrefix("items") { items =>
          // there might be no item for a given id
          val maybeItems: Future[List[Item]] = Operation.fetchAll()

          onSuccess(maybeItems) {
            case Some(item) => complete(item)
            case None       => complete(StatusCodes.NotFound)
          }
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done

  }
}

List[Student] students;
list = students.filter((el) => promedio(el.notes)) > 2.95).map((el) => el.name)