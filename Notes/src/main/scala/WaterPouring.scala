/**
  * WaterPouring problem, functional solution using Streams
 */
class WaterPouring(capacity: Vector[Int]) {

  type State = Vector[Int]

  private val initialState = capacity.map(_ => 0)

  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state.updated(glass, capacity(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from).min(capacity(to) - state(2))
      state.updated(from, state(from) - amount).updated(to, state(to) + amount)
    }
  }

  private val glasses = capacity.indices
  private val moves = (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths

  class Path(history: List[Move], val endState: State) { // most recent move saved first

    def extend(move: Move) = new Path(move :: history, move.change(endState))

    override def toString: String = history.reverse.mkString(" ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val more: Set[Path] =
        for (path <- paths;
             next <- moves.map(path.extend)
             if !explored.contains(next.endState)) yield next
      paths #:: from(more, explored ++ more.map(_.endState))
    }
  }

  private val pathSets = from(Set(initialPath), Set())

  def solution(target: Int): Stream[Path] = {
    for (pathSet <- pathSets;
         path <- pathSet
         if path.endState.contains(target)) yield path
  }

}
