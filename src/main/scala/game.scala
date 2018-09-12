import scala.collection.mutable

class Player(title: String, identity: Char) {
  val name: String = title
  val symbol: Char = identity
}

object game {
  // initialize game state
  var state: mutable.LinkedHashMap[String, Char] = scala.collection.mutable.LinkedHashMap[String, Char]("11" -> 'E',
    "12" -> 'E', "13" -> 'E', "21" -> 'E', "22" -> 'E', "23" -> 'E', "31" -> 'E', "32" -> 'E', "33" -> 'E')

  // initialize competitor data structures
  val p1 = new Player("Player 1", 'X')
  val p2 = new Player("Player 2", 'O')

  def didWin(): Boolean = {
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

  def turn(turnPlayer: Player, otherPlayer: Player): Unit = {
    // this function switches turns off until the end of the game

    // ask the player where they want to play and receive input
    println("\nWhere would you like to play, " + turnPlayer.name + " (" + turnPlayer.symbol + ")" + "?\n")
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
    println()
    for ((k, v) <- state) if (k == "13" || k == "23" || k == "33") println(v + " ") else print(v + " ")

    // check if the latest move won, tied, or another turn is necessary
    if (didWin()) println("\n" + turnPlayer.name + " wins!")
    else if (state.forall { case (_, v) => v != 'E' }) println("\nIt's a tie!")
    else turn(otherPlayer, turnPlayer)

  }

  // start the game's conditional flow
  def main(args: Array[String]): Unit = {
    turn(p1, p2)
  }
}