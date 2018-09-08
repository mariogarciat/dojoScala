import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.Done
import akka.actor.Status.Success
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.util.parsing.json._
import scala.io.StdIn
import scala.concurrent.Future

object WebServer {

  // needed to run the route
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // needed for the future map/flatmap in the end and future in fetchItem and saveOrder
  implicit val executionContext = system.dispatcher

  var orders: List[Item] = Nil

  //--------Data---------
  var note1: Note = Note(3)
  var note2: Note = Note(4.2)
  var note3: Note = Note(3.3)
  var note4: Note = Note(4)
  var note5: Note = Note(2.5)

  var student1 = Student("Juan", "1", List(note1, note2, note3))
  var student2 = Student("Andres", "2", List(note4, note3, note5))
  var student3 = Student("Andres", "2", List(note1, note4, note5))
  var student4 = Student("Julian", "5", List(note5, note3, note5))
  var student5 = Student("Juan", "1", List(note5, note3, note3))
  var student6 = Student("Jose", "3", List(note3, note4, note2))
  var student7 = Student("Miguel", "3", List(note2, note2, note3))
  var student8 = Student("Miguel", "3", List(note1, note4, note1))

  var course1 = Course(List(student3, student1, student5, student4, student8), "Curso 1", "111")
  var course2 = Course(List(student5, student1, student6, student2), "Curso 2", "222")
  var course3 = Course(List(student3, student1, student7), "Curso 3", "333")
  var courses: List[Course] =  List(course1, course2, course3)

  // domain model
  final case class Item(name: String, id: Long)
  final case class Order(items: List[Item])


  final case class Student(name: String, id: String, notes: List[Note])
  final case class Note(value: Double)
  final case class Course(students: List[Student], name: String, id: String)


  // formats for unmarshalling and marshalling
  implicit val itemFormat = jsonFormat2(Item)
  implicit val orderFormat = jsonFormat1(Order)


  implicit val noteFormat = jsonFormat1(Note)
  object MyJsonStudentProtocol extends DefaultJsonProtocol {
    implicit def studentFormat[Note :JsonFormat] = jsonFormat3(Student.apply)
  }
  object MyJsonCourseProtocol extends DefaultJsonProtocol {
    implicit def courseFormat[Student :JsonFormat] = jsonFormat3(Student.apply)
  }


  // (fake) async database query api

  object Operation {

    var students: List[Student] = Nil

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

    def getWinnerStudents(allStudents: List[Student]): Future[List[String]] = Future{

      allStudents.filter(stu => average(stu.notes) > 2.95)
        .map(stu => stu.name)

    }

    def average(notes: List[Note]): Double = {
      val notesValues: List[Double] = notes.map(el=>el.value)
      val sum = notesValues.foldRight(0.0){(acc,el) => acc+el}
      sum/notes.length
    }

    /*def addNote(note: Double, studentId: String, courseId: String): Future[Done] = {
     val student: Option[Student] = courses.find(c => c.id == courseId)
                                    .flatMap(c => c.students
                                      .find(e => e.id == studentId))
     var studentNotes = student.get.notes

    }*/

    def fetchStudents(courseId: String): Future[Option[List[Student]]] = Future {
      courses.find(c => c.id == courseId).flatMap(course => Option(course.students))
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
        pathPrefix("courses" / IntNumber) {
           courseId =>
            // there might be no item for a given id
            val maybeStudents: Future[Option[List[Student]]] = Operation.fetchStudents(courseId.toString)


            onSuccess(maybeStudents) {
              case Some(some) => complete(some.toString)
              case None => complete(StatusCodes.NotFound)
            }

        }
      }~
      get {
        pathPrefix("winners") { courseId => {
          val students: Future[List[String]] = Operation.getWinnerStudents(course1.students)

          onSuccess(students) {
            case Some(students) => complete(students.toString)
          }




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
