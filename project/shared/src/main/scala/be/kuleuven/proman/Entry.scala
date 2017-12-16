package be.kuleuven.proman

import scalatags.generic.Bundle

case class Entry(id: Int, var done: Boolean, var content: String) {

  def hasID(searchId: Int): Boolean = searchId == id
}

class EntryTemplate[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def entryDivTemplate(boardName: String, e: Entry) =
    div(id := boardName + e.id)

  def entryTemplate(e: Entry) =
    p(e.content)

  def entryInputTemplate(boardName: String, e: Entry) =
    input(
      id := boardName + e.id + "Input",
      tpe := "text",
      value := e.content,
      border := "none",
      background := "transparent"
    )

  def entryButtonTemplate(e: Entry) =
    if (e.done) button("Mark as to do")
    else button("Mark as done")

  def boardTemplate(boardName: String) =
    div(
      textAlign := "center",
      padding := "30px",
      backgroundColor:= "green")(
      h3(
        boardName,
        textTransform := "uppercase"),
      div(
        id := boardName + "Entries",
        justifyContent := "center") (
        div(
          margin := "5px",
          padding := "10px",
          width := "400px",
          backgroundColor := "deepskyblue")(
          div (
            id := boardName + "TodoEntriesTitle",
            textAlign :="center",
            cursor := "pointer")(
            h4("To do")
          ),
          div(id := boardName+"TodoEntries")
        ),
        div(
          margin := "5px",
          padding := "10px",
          width := "400px",
          backgroundColor := "deepskyblue")(
          div (
            id := boardName + "DoneEntriesTitle",
            textAlign:="center", cursor := "pointer")(
            h4("Done")
          ),
          div(id := boardName + "DoneEntries")
        )
      ),
      br(),
      div(
        input(id := boardName + "EntryName",
          tpe := "text",
          placeholder := "New Entry"),
        button(id := boardName + "AddEntry", "Add")
      )
    )
}
