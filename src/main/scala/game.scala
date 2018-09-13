import scala.collection.mutable

class Player(title: String, identity: Char, AI: Boolean) {
  val name: String = title
  val symbol: Char = identity
  val isComputer: Boolean = AI
}

object game {

  def main(args: Array[String]): Unit = {
    // start the game's conditional flow
    mainMenu()
  }

  def mainMenu(): Unit = {

    println("\nWelcome to Tic Tac Text. What would you like to do? Type the number that is beside your selection.\n" +
      "\n1. 1 Player\n2. 2 Player\n3. How to Play\n")

    // take the user's response and launch into the desired selection
    val response: String = scala.io.StdIn.readLine()
    if (response == "1") {
      gameFlow(false)
    } else if (response == "2") {
      gameFlow(true)
    } else if (response == "3") {
      gameExplanation()
    } else {
      println("That is not a valid option."); mainMenu()
    }
  }

  def gameFlow(is2Player: Boolean): Unit = {
    // initialize game state
    val gameState: mutable.LinkedHashMap[String, Char] = scala.collection.mutable.LinkedHashMap[String, Char]("11" -> 'E',
      "12" -> 'E', "13" -> 'E', "21" -> 'E', "22" -> 'E', "23" -> 'E', "31" -> 'E', "32" -> 'E', "33" -> 'E')

    // initialize player data structures
    val p1 = new Player("Player 1", 'X', false)
    val p2 = new Player("Player 2", 'O', false)
    val p3 = new Player("The Computer", 'O', true)

    for ((k, v) <- gameState) if (k == "13" || k == "23" || k == "33") println(v + " ") else print(v + " ")

    if (is2Player) {
      // print the winner if there is one; if not, proclaim a tie and ask if another game is desired
      try {
        println("\n" + turnFlow(p1, p2, gameState).get.name + " wins!")
      }
      catch {
        case _: Throwable => println("\nIt's a tie!")
      }
      finally {
        mainMenu()
      }
    }
    else
      // print the winner if there is one; if not, proclaim a tie and ask if another game is desired
      try {
        println("\n" + turnFlowAI(p1, p3, gameState).get.name + " wins!")
      }
      catch {
        case _: Throwable => println("\nIt's a tie!")
      }
      finally {
        mainMenu()
      }
  }

  def turnFlowAI(humPlayer: Player, aiPlayer: Player, state: mutable.LinkedHashMap[String, Char]): Option[Player] = {
    println("Still under construction! Come back another time!"); Option(humPlayer)
  }

  def turnFlow(turnPlayer: Player, otherPlayer: Player, state: mutable.LinkedHashMap[String, Char]): Option[Player] = {
    // this function switches turns off until the end of the game

    // ask the player where they want to play and receive input
    println("\nWhere would you like to play, " + turnPlayer.name + " (" + turnPlayer.symbol + ")" + "? (type row# then column# eg. '11' for top-left)\n")
    var move = scala.io.StdIn.readLine()

    // check if it is a valid move and loop until a legitimate move is performed
    while ((move != "11" && move != "12" && move != "13" && move != "21" && move != "22" && move != "23" && move != "31"
      && move != "32" && move != "33") || state(move) == otherPlayer.symbol || state(move) == turnPlayer.symbol) {

      println("\nThat space is already taken or does not exist. Try again.\n")
      move = scala.io.StdIn.readLine()

    }

    // update game state with valid move
    state(move) = turnPlayer.symbol

    // visualize updated game state
    for ((k, v) <- state) if (k == "13" || k == "23" || k == "33") println(v + " ") else print(v + " ")

    // check if the latest move won, tied, or another turn is necessary
    if (didWin(state)) Option(turnPlayer)
    else if (state.forall { case (_, v) => v != 'E' }) None
    else turnFlow(otherPlayer, turnPlayer, state)
  }

  def didWin(state: mutable.LinkedHashMap[String, Char]): Boolean = {
    // this function returns true if a win condition is met
    if (state("11") == state("12") && state("11") == state("13") && state("11") != 'E') true
    else if (state("21") == state("22") && state("21") == state("23") && state("21") != 'E') true
    else if (state("31") == state("32") && state("31") == state("33") && state("31") != 'E') true
    else if (state("11") == state("21") && state("11") == state("31") && state("11") != 'E') true
    else if (state("12") == state("22") && state("12") == state("32") && state("12") != 'E') true
    else if (state("13") == state("23") && state("13") == state("33") && state("13") != 'E') true
    else if (state("11") == state("22") && state("11") == state("33") && state("11") != 'E') true
    else if (state("13") == state("22") && state("13") == state("31") && state("13") != 'E') true
    else false
  }

  def gameExplanation(): Unit = {

    println("\nThis is a game of Tic Tac Toe, played via the command line.\nYou can either play against an AI opponent" +
      "or someone on your local machine by switching turns.\nTo make a move, you type the row number, counted from top" +
      " to bottom, followed immediately by the column number, counted from left to right.\nFor example, to make a move" +
      " in the top-right, you would type '13', and to make a move in the middle, you would type '22', and so on.")

    mainMenu()
  }
}