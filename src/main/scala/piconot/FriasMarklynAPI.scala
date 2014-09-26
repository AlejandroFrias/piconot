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
    var section_number = 0
    var line_number = 0
    var rules: List[Rule] = List() //TODO so ugly

    var sections: List[String] = List()
    
    def Sections(args: String*): Unit = {
        sections = args.toList
    }


    trait FaceTrait {
        def up = genFace(1)
        def right = genFace(2)
        def down = genFace(3)
        def left = genFace(4)

        def genFace(toFace: Int): Unit = {
        line_number = line_number + 1
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
    }

    object Face extends FaceTrait

    trait TurnTrait {
        def left = genTurn(-1)
        def right = genTurn(1)
        def around = genTurn(2)

        def genTurn(turnAmount: Int): Unit = {
            line_number = line_number + 1;
            rules = rules ++ List(
                Rule(State(section_number + "0" + line_number + "01"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(section_number + "0" + (line_number + 1)+ "0" + toDir(1 + turnAmount))
                    ),
                Rule(State(section_number + "0" + line_number + "02"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(section_number + "0" + (line_number + 1)+ "0" + toDir(2 + turnAmount))
                    ),
                Rule(State(section_number + "0" + line_number + "03"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(section_number + "0" + (line_number + 1)+ "0" + toDir(3 + turnAmount))
                    ),
                Rule(State(section_number + "0" + line_number + "04"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(section_number + "0" + (line_number + 1)+ "0" + toDir(4 + turnAmount))
                    )
                )
        }
    }

    object Turn extends TurnTrait

    def toDir(in: Int) = ((in - 1) % 4) + 1


    object New {
        def Section(label: String): Unit = {
            section_number = sections.indexOf(label) + 1
            line_number = 0
        }
    }

    object Do {
        def Section(label: String): Unit = {
            line_number = line_number + 1
            val new_section = sections.indexOf(label) + 1
            rules = rules ++ List(
                Rule(State(section_number + "0" + line_number + "01"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(new_section + "0101")
                    ),
                Rule(State(section_number + "0" + line_number + "02"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(new_section + "0102")
                    ),
                Rule(State(section_number + "0" + line_number + "03"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(new_section + "0103")
                    ),
                Rule(State(section_number + "0" + line_number + "04"),
                    Surroundings(Anything, Anything, Anything, Anything),
                    StayHere,
                    State(new_section + "0104")
                    )
                )
        }
    }



}



// Stretch Goals:
//    # comments instead of //
//    If statements
//    spaces
//    pre-parsing to get rid of "" and {}
