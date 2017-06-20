package hello

import com.company.immutable.LinkedList

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, Priority, VBox}

object LinkedListApp extends JFXApp {

  private val listView: ListView[BigInt] = createListView()
  private var sourceList = new LinkedList[BigInt]

  stage = new PrimaryStage {

    title = "Linked List"
    scene = new Scene(375, 450) {
      resizable = false


      content = new VBox() {
        children = Seq(
          listView,
          createButtonBarAndTextArea()
        )
      }
    }

  }

  private def createListView(): ListView[BigInt] = {

    new ListView[BigInt] {
      margin = Insets(10)
      orientation = Orientation.Horizontal
      prefHeight = 100
      prefWidth = 280
    }
  }

  private def createButtonBarAndTextArea(): GridPane = {

    val logArea = new TextArea() {
      prefWidth = 280
      prefHeight = 125
      editable = false
    }
    GridPane.setConstraints(logArea, 0, 5, 2, 1)

    val valLabel = new Label() {
      text = "Integer value:"
    }
    GridPane.setConstraints(valLabel, 0, 0)

    val textField = new TextField() {
      prefWidth = 100
    }
    GridPane.setConstraints(textField, 1, 0)

    val addButton = new Button() {
      text = "Add value in the end"
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
      onAction = handle {
        if (isValidBigInt(textField.text.value)) {
          val value = BigInt(textField.text.value)
          sourceList = sourceList.add(value)
          listView.items = ObservableBuffer(sourceList)
          logArea.setText(logArea.text.value + s"\n Added the value $value")
        }
      }
      disable <== textField.text.isEmpty

    }
    GridPane.setConstraints(addButton, 0, 1)

    val insertButton = new Button() {
      text = "Insert value after selected"
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
      onAction = handle {
        if (isValidBigInt(textField.text.value)) {
          val value = BigInt(textField.text.value)
          val pos = listView.selectionModel.value.getSelectedIndex + 1
          sourceList = sourceList.insert(pos, value)
          listView.items = ObservableBuffer(sourceList)
          logArea.setText(logArea.text.value + s"\n Inserted the value $value at position $pos")
        }
      }
      disable <== textField.text.isEmpty
    }
    GridPane.setConstraints(insertButton, 1, 1)

    val deleteButton = new Button() {
      text = "Delete selected"
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
      onAction = handle {
        val pos = listView.selectionModel.value.getSelectedIndex
        sourceList = sourceList.remove(pos)
        listView.items = ObservableBuffer(sourceList)
        logArea.setText(logArea.text.value + s"\n Deleted the value at position $pos")
      }
    }
    GridPane.setConstraints(deleteButton, 0, 2)

    val updateButton = new Button() {
      text = "Update selected with value"
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
      onAction = handle {
        if (isValidBigInt(textField.text.value)) {
          val value = BigInt(textField.text.value)
          val pos = listView.selectionModel.value.getSelectedIndex
          sourceList = sourceList.set(pos, value)
          listView.items = ObservableBuffer(sourceList)
          logArea.setText(logArea.text.value + s"\n Updated position $pos with value $value")
        }
        disable <== textField.text.isEmpty
      }
    }
    GridPane.setConstraints(updateButton, 1, 2)

    val sortButton = new Button() {
      text = "Sort (ascending)"
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
      onAction = handle {
        sourceList.accept(new LinkedListSortLogVisitor[BigInt](logArea))
        logArea.setText(logArea.text.value + s"\n Sort started")
        sourceList = sourceList.mergeSort()
        logArea.setText(logArea.text.value + s"\n Sort completed successfully")
        listView.items = ObservableBuffer(sourceList)
      }
    }
    GridPane.setConstraints(sortButton, 0, 3, 2, 1)

    val textAreaLabel = new Label() {
      text = "Log:"
    }
    GridPane.setConstraints(textAreaLabel, 0, 4, 2, 1)

    val clearButton = new Button() {
      text = "Clear Log"
      maxWidth = Double.MaxValue
      hgrow = Priority.Always
      onAction = handle {
        logArea.setText("")
      }
    }
    GridPane.setConstraints(clearButton, 0, 6, 2, 1)


    new GridPane() {
      hgap = 4
      vgap = 6
      margin = Insets(10)
      children = Seq(valLabel, textField, addButton, insertButton, deleteButton, updateButton, sortButton, textAreaLabel,
        logArea, clearButton)
    }

  }

  private def isValidBigInt(number: String): Boolean = {
    try {
      BigInt(number)
      true
    }
    catch {
      case nfe: NumberFormatException => {
        new Alert(AlertType.Error, s"$number is not a valid Integer").showAndWait()
      }
        false
    }
  }

}
