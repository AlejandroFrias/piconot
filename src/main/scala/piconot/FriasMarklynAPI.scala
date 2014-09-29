package piconot


import java.io.File

import picolib.maze.Maze
import picolib.semantics._
// import picolib.semantics.Anything
// import picolib.semantics.Blocked
// import picolib.semantics.East
// import picolib.semantics.GUIDisplay
// import picolib.semantics.North
// import picolib.semantics.Open
// import picolib.semantics.Picobot
// import picolib.semantics.Rule
// import picolib.semantics.South
// import picolib.semantics.State
// import picolib.semantics.StayHere
// import picolib.semantics.Surroundings
// import picolib.semantics.TextDisplay
// import picolib.semantics.West
import scalafx.application.JFXApp


class FriasMarklynAPI(val mapFile: String) {
    var section_number = 0
    var line_number = 0
    var rules: List[Rule] = List() //TODO so ugly

    var sections: List[String] = List()

    val dirToMoveDirection = Map( 1 -> North, 2 -> East, 3 -> South, 4 -> West )
    
    def Sections(args: String*): Unit = {
        sections = args.toList
    }

    val anySurroundings = Surroundings(Anything, Anything, Anything, Anything)


    trait FaceTrait {
        def up = genFace(1)
        def right = genFace(2)
        def down = genFace(3)
        def left = genFace(4)

        def genFace(toFace: Int): Unit = {
        line_number = line_number + 1
        val newRules = List.range(1,5).map(dir => makeRule(
            section_number, line_number, dir,
            anySurroundings, StayHere,
            section_number, line_number + 1, toFace
            ))
        rules = rules ++ newRules
        }
    }

    object Face extends FaceTrait

    trait TurnTrait {
        def left = genTurn(-1)
        def right = genTurn(1)
        def around = genTurn(2)

        def genTurn(turnAmount: Int): Unit = {
            line_number = line_number + 1;
            val newRules = List.range(1,5).map(dir => makeRule(
                section_number, line_number, dir,
                anySurroundings, StayHere,
                section_number, line_number + 1, toDir(dir + turnAmount)
                ))
            rules = rules ++ newRules
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
            val newRules = List.range(1,5).map( dir =>
                makeRule(section_number, line_number, dir,
                anySurroundings, StayHere,
                new_section, 1, dir) )
            rules = rules ++ newRules
        }
    }

    val once_if_possible: List[Surroundings] = List()

    def whilst(conds: Map[Int, RelativeDescription]*): List[Surroundings] = {
        val m = conds.reduce(_ ++ _).withDefaultValue(Anything)
        List.range(0,4).map( dir =>  // Don't pay too much attention to these numbers -- So much magic
            Surroundings(m((4 - dir) %4), m((5 - dir) %4), m((7 - dir) %4), m((6 - dir) %4)) ) 
    }

    object Go {
        def forwards(conds: List[Surroundings]) = makeGo(0, conds)

        def right(conds: List[Surroundings]) = makeGo(1, conds)

        def backwards(conds: List[Surroundings]) = makeGo(2, conds)

        def left(conds: List[Surroundings]) = makeGo(3, conds)

        private def makeGo(dirDiff: Int, conds: List[Surroundings]) = {
            line_number = line_number + 1
            conds match {
                case Nil => rules = rules ++ List.range(1,5).flatMap( dir => 
                                makeOnceRules(dir, toDir(dir + dirDiff)))

                
                case any => rules = rules ++ List.range(1,5).flatMap( dir =>
                    rulesFromDirAndSurr(dir, toDir(dir + dirDiff), conds(dir-1)))
            }
        }

        private def makeOnceRules(dirFacing: Int, moveDir: Int) = dirToMoveDirection(moveDir) match {
            case North => List( makeRule(section_number, line_number, dirFacing,
                                Surroundings(Open, Anything, Anything, Anything), North,
                                section_number, line_number + 1, dirFacing ),
                                makeRule(section_number, line_number, dirFacing,
                                Surroundings(Blocked, Anything, Anything, Anything), StayHere,
                                section_number, line_number + 1, dirFacing ))

            case East => List( makeRule(section_number, line_number, dirFacing,
                                Surroundings(Anything, Open, Anything, Anything), East,
                                section_number, line_number + 1, dirFacing ),
                                makeRule(section_number, line_number, dirFacing,
                                Surroundings(Anything, Blocked, Anything, Anything), StayHere,
                                section_number, line_number + 1, dirFacing ))

            case South => List( makeRule(section_number, line_number, dirFacing,
                                Surroundings(Anything, Anything, Anything, Open), South,
                                section_number, line_number + 1, dirFacing ),
                                makeRule(section_number, line_number, dirFacing,
                                Surroundings(Anything, Anything, Anything, Blocked), StayHere,
                                section_number, line_number + 1, dirFacing ))

            case West => List( makeRule(section_number, line_number, dirFacing,
                                Surroundings(Anything, Anything, Open, Anything), West,
                                section_number, line_number + 1, dirFacing ),
                                makeRule(section_number, line_number, dirFacing,
                                Surroundings(Anything, Anything, Blocked, Anything), StayHere,
                                section_number, line_number + 1, dirFacing ))
            
        }

                                        

        private def rulesFromDirAndSurr(dirFacing: Int, moveDir: Int, surr: Surroundings): List[Rule] = {
            var someRules: List[Rule] = List(makeRule(section_number, line_number, dirFacing,
                                             surr, dirToMoveDirection(moveDir),
                                             section_number, line_number, dirFacing))
            

            if(surr.north != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(opposite(surr.north), Anything, Anything, Anything), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            if(surr.east != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(Anything, opposite(surr.east), Anything, Anything), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            if(surr.south != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(Anything, Anything, Anything, opposite(surr.south)), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            if(surr.west != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(Anything, Anything, opposite(surr.west), Anything), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            someRules
        }

        private def opposite(relDesc: RelativeDescription): RelativeDescription = {
            if(relDesc == Blocked) {
                Open
            } else {
                Blocked
            }
            
        }
    }

    object open {
        def in_front = Map(0 -> Open)
        def on_right = Map(1 -> Open)
        def behind = Map(2 -> Open)
        def on_left = Map(3 -> Open)
    }

    object wall {
        def in_front = Map(0 -> Blocked)
        def on_right = Map(1 -> Blocked)
        def behind = Map(2 -> Blocked)
        def on_left = Map(3 -> Blocked)
    }




    def makeRule(start_section: Int, start_line: Int, start_dir: Int,
                 surroundings: Surroundings, move_dir: MoveDirection,
                 end_section: Int, end_line: Int, end_dir: Int) = {
        Rule(State(start_section + "0" + start_line + "0" + start_dir),
                    surroundings,
                    move_dir,
                    State(end_section + "0" + end_line + "0" + end_dir)
                    )
    }

    class RunApp(val some_rules: List[Rule], val map: String) extends JFXApp {
        val emptyMaze = Maze("resources" + File.separator + map)

         object EmptyBot extends Picobot(emptyMaze, some_rules)
             with TextDisplay with GUIDisplay

         stage = EmptyBot.mainStage
    }

    def main(args: Array[String]): Unit = {
      val app = new RunApp(rules, mapFile)
      app.main(Array())
    }
    
}



// Stretch Goals:
//    # comments instead of //
//    If statements
//    spaces
//    pre-parsing to get rid of "" and {}
