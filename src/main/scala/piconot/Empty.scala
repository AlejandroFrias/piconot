package piconot

import scala.language.postfixOps

import java.io.File

import picolib.maze.Maze
import picolib.semantics.Anything
import picolib.semantics.Blocked
import picolib.semantics.East
import picolib.semantics.GUIDisplay
import picolib.semantics.North
import picolib.semantics.Open
import picolib.semantics.Picobot
import picolib.semantics.Rule
import picolib.semantics.South
import picolib.semantics.State
import picolib.semantics.StayHere
import picolib.semantics.Surroundings
import picolib.semantics.TextDisplay
import picolib.semantics.West
import scalafx.application.JFXApp



object EmptyRules extends FriasMarklynAPI {

    Sections("start", "Spin");

    New Section "start";
        Face down;
        Do Section "Spin"

    New Section "Spin";
        Turn right;
        Go forwards once;
        Go left once;
        Do Section "Spin";

}

object Empty extends JFXApp{


     val emptyMaze = Maze("resources" + File.separator + "empty.txt")

     object EmptyBot extends Picobot(emptyMaze, EmptyRules.rules)
         with TextDisplay with GUIDisplay

     stage = EmptyBot.mainStage
}