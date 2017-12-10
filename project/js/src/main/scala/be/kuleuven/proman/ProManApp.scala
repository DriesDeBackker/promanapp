package be.kuleuven.proman

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{Event, HTMLButtonElement, HTMLInputElement}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object ProManApp {
  def main(args: Array[String]): Unit = {

    // STEP 4: Embed this program into your HTML and make sure it loads.
    // dom.window.alert("YOUR JAVASCRIPT LOADED")

    // STEP 5: Change your HttpService so that it defines a second page.
    // This page will be a copy of the first page but instead of generating
    // server-side HTML you will load the animals client-side using the Javascript APIs.

    def update() =
      Ajax.get("/service/animal").onComplete {
        case Success(xhr) =>
          val animalsM = decode[Seq[Animal]](xhr.responseText)

          animalsM match {
            case Left(err) => dom.window.alert(err.toString)
            case Right(animals) =>
              val animalTarget = dom.document.body.querySelector("#animals")
              animalTarget.innerHTML = ""
              val ui = new AnimalTemplate(scalatags.JsDom)
              animalTarget.appendChild(ui.animalsTemplate(animals).render)
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    update()

    val addDogEl = dom.document.body
      .querySelector("#addDog")
      .asInstanceOf[HTMLButtonElement]

    addDogEl.onclick = (ev: Event) => {
      val dogNameEl = dom.document.body
        .querySelector("#dogName")
        .asInstanceOf[HTMLInputElement]
      val dogName = dogNameEl.value

      Ajax
        .post("/service/animal", Animal("dog", dogName).asJson.noSpaces)
        .onComplete {
          case Success(xhr) =>
            dogNameEl.value = ""
            // This is a very, VERY crappy way of updating, don't do this in
            // your  project, come up with something that doesn't redraw the entire tree!
            update()
          case Failure(err) => dom.window.alert(err.toString)
        }
    }
  }

}
