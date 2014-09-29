package piconot

import scala.language.postfixOps

import java.io.File

import picolib.maze.Maze
import picolib.semantics.GUIDisplay
import picolib.semantics.Picobot
import picolib.semantics.TextDisplay
import scalafx.application.JFXApp



object RightHand extends FriasMarklynAPI("maze.txt") {

    Sections("Right Hand Rule");

    New Section "Right Hand Rule";
        Go forwards whilst(wall on_right, open in_front);
        Turn right;
        Go forwards once_if_possible;
        Turn around;
        Do Section "Right Hand Rule";

}