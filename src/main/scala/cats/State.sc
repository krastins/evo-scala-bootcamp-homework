import cats.data.{State, StateT}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import cats.instances.future._

import scala.concurrent.duration.Duration

/*
Note: you can run this as Scala worksheet in intelliJ idea if you want interactive examples

State is a structure that provides a functional approach to handling application state.

State[S, A] is basically a function S => (S, A), where S is the type that represents your state
and A is the result the function produces.

In addition to returning the result of type A, the function returns a new S value, which is the updated state.

With State you can use pure functions to transform application state, by passing around the state along with computations.
*/

/*
A canonical example is the (pseudo) random number generator (RNG) that needs a seed.

In an imperative program, every time you would generate a random number,
you would also mutate the the seed within the RNG, usually stored in a global variable.

For randomNumber to be a pure function, you need to pass it the seed, and return a new seed along with the random number,
so that we can generate the next random number. State helps to model this.

I'll be using scala.util.Random for simplicity, but check out Cats doc[1] to see a neat way to do this without using it.
*/

def randomNumber: State[Long, Int] = State[Long, Int] { seed =>
  val rng = new scala.util.Random(seed)
  val randomNumber = rng.nextInt()
  val newSeed = rng.nextLong()
  (newSeed, randomNumber)
}

/*
State.run returns a `cats.Eval`
you can call .value on it to get the a tuple of new state and resulting value from the computation.
*/

val initialSeed = 0L
val (newSeed: Long, randomInteger: Int) = randomNumber.run(initialSeed).value

/*
If you only care about the resulting state you can use `runS`
or use `runA` if you only care about the value of computation
*/

val next = randomNumber.runA(newSeed).value

/*
State's `map` allows to map a function on the resulting value, without changing the state
*/

val randomBoolean: Boolean = randomNumber.map(int => int >= 0).runA(initialSeed).value

/*
For another example, let's define a repository of players using State
 */

case class Player(id: Int, name: String, score: Int)

object PlayerRepo {
  def create(player: Player) =
    State[List[Player], Player] { players =>
      (player :: players, player)
    }

  def find(id: Int) =
    State[List[Player], Option[Player]] { players =>
      (players, players.find(_.id == id))
    }

  def update(player: Player) =
    State[List[Player], Option[Player]] { players =>
      (players.map(old => if (old.id == player.id) player else old),
        players.find(_.id == player.id))
    }

  def getAll():State[List[Player], List[Player]] = State.get
}

/*
`flatMap` allows to use State in for comprehensions to hide the complexity of passing around the state
 */

val stateManipulation = for {
  p1 <- PlayerRepo.create(Player(1, "a", 10))
  p2 <- PlayerRepo.create(Player(2, "b", 20))
  p3 <- PlayerRepo.create(Player(3, "c", 30))
  found <- PlayerRepo.find(2)
  updated <- PlayerRepo.update(found.get.copy(score = 40))
  players <- PlayerRepo.getAll()
} yield players.map(_.score).sum

/*
All of the manipulations are only executed after we pass the initial state (empty list) and call the `value` function.
 */
val (finalState: List[Player], scoreSum: Int) = stateManipulation.run(List()).value

/*
What if our update methods are async e.g. returning a Future or IO?
 */
def updateAsync(players: List[Player], player: Player): Future[(List[Player], Option[Player])] =
  Future.successful(
    (
      players.map(old => if (old.id == player.id) player else old),
      players.find(_.id == player.id)
    )
  )

/*
We can use StateT[F[_], S, A] – a state transformer
*/

object AsyncPlayerRepo {

  def update(player: Player): StateT[Future, List[Player], Option[Player]] =
    StateT { players: List[Player] => updateAsync(players, player) }

/*
Let's also take a look at some additional functions offered by State and StateT:
get – keeps the state as is, and returns it.
set(s) – overwrites the state with s and returns ().
inspect(f) – applies the function f: S => T to s, and returns the result without modifying the state itself.
modify(f) applies the function f: S => T to s, saves the result as the new state, and returns ().
*/
  def bumpAllScores(amount: Int): StateT[Future, List[Player], Unit] = {
    StateT.modify {
      _.map(p => p.copy(score = p.score + amount))
    }
  }

  def findTopPlayer(): StateT[Future, List[Player], Option[Player]] = {
    def Descending[T: Ordering] = implicitly[Ordering[T]].reverse
    StateT.inspect(players => players.sortBy(_.score)(Descending).headOption)
  }

  def modify = {
    for {
      p1 <- update(Player(2, "b", 50))
      p2 <- update(Player(3, "c", 0))
      _ <- bumpAllScores(15)
      top <- findTopPlayer()
    } yield top
  }
}

val initialState = List(Player(1, "a", 10), Player(2, "b", 20), Player(3, "c", 30))
val resultFuture = AsyncPlayerRepo.modify.run(initialState)
val (finalState: List[Player], topPlayer: Option[Player]) = Await.result(resultFuture, Duration.Inf)

/*
In fact State[S, A] is actually StateT[Eval, S, A] under the hood.

StateT is stack safe, which means the value will be run without stackoverflow even for very long flatMap sequences
*/

/*
Sources/additional material:
[1] https://typelevel.org/cats/datatypes/state.html
[2] https://eed3si9n.com/herding-cats/State.html
[3] https://www.youtube.com/watch?v=Pgo73GfHk0U Regaining Control with State Monad and Friends (Felix Mulder)
 */
