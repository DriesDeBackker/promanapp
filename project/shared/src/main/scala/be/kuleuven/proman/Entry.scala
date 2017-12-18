package be.kuleuven.proman

import scalatags.generic.Bundle

case class Entry(id: Int, date: String, var done: Boolean, var content: String) {

  def hasID(searchId: Int): Boolean = searchId == id
}

class EntryTemplate[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) {

  import bundle.all._

  def entryDivTemplate(boardName: String, e: Entry) =
    div(id := boardName + e.id,
      display:= "flex")

  def entryTemplate(e: Entry) =
    p(e.content, fontSize := "10px")

  def entryDateTemplate(boardName: String, e: Entry) =
    div(e.date,
      width:="70px",
      padding:= "6px",
      fontSize:="11px")

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
      margin := "15px",
      padding := "30px",
      backgroundColor := "green")(
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
          div(id := boardName+"TodoEntries",
              visibility := "visible")
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
          div(id := boardName + "DoneEntries",
              visibility := "visible")
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
