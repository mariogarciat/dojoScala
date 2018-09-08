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
  var

  // domain model
  final case class Item(name: String, id: Long)
  final case class Order(items: List[Item])

  final case class Student(name: String, id: String, notes: List[Note])
  final case class Note(value: Double)
  final case class Course(name: String, id: String, students: List[Student])


  // formats for unmarshalling and marshalling
  //implicit val itemFormat = jsonFormat2(Item)
  //implicit val orderFormat = jsonFormat1(Order)

  implicit val studentFormat = jsonFormat3(Student)
  implicit val noteFormat = jsonFormat1(Note)
  implicit val courseFormat = jsonFormat3(Course)

  // (fake) async database query api

  object Operation {

    var students: List[Student] = Nil
    var courses: List[Course] = List()

    def fetchItem(itemId: Long): Future[Option[Item]] = Future {
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
    }

    def addStudent(student: Student, idCourse: String): Future[Done] = {

       //student::course.students
      val course: Option[Course] = courses.find(c => c.id == idCourse)
      var courseStudents = course.get.students
      courseStudents = student match {
        case Student(name,id,notes) => student::courseStudents
        case _ => courseStudents
      }
      Future {
        Done
      }
    }

    def getWinnerStudents(allStudents: List[Student]): List[String] = {

      allStudents.filter(stu => average(stu.notes) > 2.95)
        .map(stu => stu.name)

    }

    def average(notes: List[Note]): Double = {
      val notesValues: List[Double] = notes.map(el=>el.value)
      val sum = notesValues.foldRight(0.0){(acc,el) => acc+el}
      sum/notes.length
    }

    def addNote(note: Double, studentId: String, courseId: String): Future[Done] = {
     val student: Option[Student] = courses.find(c => c.id == courseId)
                                    .flatMap(c => c.students
                                      .find(e => e.id == studentId))
     var studentNotes = student.get.notes

    }

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