package piconot


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


class FriasMarklynAPI {
    var section_number = 1
    var line_number = 0
    var rules: List[Rule] = List() //TODO so ugly

    def genFace(toFace: Int): Unit = {
        rules = rules ++ List(
            Rule(State(section_number + "0" + line_number + "01"),
                Surroundings(Anything, Anything, Anything, Anything),
                StayHere,
                State(section_number + "0" + (line_number + 1)+ "0" + toFace)
                ),
            Rule(State(section_number + "0" + line_number + "02"),
                Surroundings(Anything, Anything, Anything, Anything),
                StayHere,
                State(section_number + "0" + (line_number + 1)+ "0" + toFace)
                ),
            Rule(State(section_number + "0" + line_number + "03"),
                Surroundings(Anything, Anything, Anything, Anything),
                StayHere,
                State(section_number + "0" + (line_number + 1)+ "0" + toFace)
                ),
            Rule(State(section_number + "0" + line_number + "04"),
                Surroundings(Anything, Anything, Anything, Anything),
                StayHere,
                State(section_number + "0" + (line_number + 1)+ "0" + toFace)
                )
            )
        }

    class FaceClass(val section_number: Int, val line_number: Int) {
        def up = genFace(1)
        def right = genFace(2)
        def down = genFace(3)
        def left = genFace(4)
    }

    def Face = {
        line_number = line_number + 1 //TODO so ugly
        new FaceClass(section_number, line_number)
    }
}

abstract class Command(val section_number: Int, val line_number: Int) {
    
}

// trait FaceTrait {

//     def genFace(toFace: Int) : List[Rule] = List(
//             Rule(State(section_number + "0" + line_number + "01"),
//                 Surroundings(Anything, Anything, Anything, Anything),
//                 North,
//                 StayHere,
//                 State(section_number + "0" + (line_number + 1)+ "0" + toFace)
//                 ),
//             Rule(State(section_number + "0" + line_number + "02"),
//                 Surroundings(Anything, Anything, Anything, Anything),
//                 North,
//                 StayHere,
//                 State(section_number + "0" + (line_number + 1)+ "0" + toFace)
//                 ),
//             Rule(State(section_number + "0" + line_number + "03"),
//                 Surroundings(Anything, Anything, Anything, Anything),
//                 North,
//                 StayHere,
//                 State(section_number + "0" + (line_number + 1)+ "0" + toFace)
//                 ),
//             Rule(State(section_number + "0" + line_number + "04"),
//                 Surroundings(Anything, Anything, Anything, Anything),
//                 North,
//                 StayHere,
//                 State(section_number + "0" + (line_number + 1)+ "0" + toFace)
//                 ))

//     class FaceClass(val section_number: Int, val line_number: Int) {
//         def up = genFace(1)
//         def right = genFace(2)
//         def down = genFace(3)
//         def left = genFace(4)
//     }

//     def Face = new FaceClass(section_number, line_number) //TODO increment?

// }

trait Turn extends Command {

}

trait Go extends Command {

}

trait Do extends Command {

}

trait Section {

    // implicit def String2Section(label: String): Section = new Section
}


// Stretch Goals:
//    # comments instead of //
//    If statements
//    spaces
//    pre-parsing to get rid of "" and {}
