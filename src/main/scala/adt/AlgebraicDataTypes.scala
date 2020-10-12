package adt

object AlgebraicDataTypes {

  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  sealed trait Suit
  object Suit {
    final case object Diamonds extends Suit
    final case object Clubs extends Suit
    final case object Hearts extends Suit
    final case object Spades extends Suit
  }

  sealed trait Rank
  object Rank {
    final case object Ace extends Rank
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
  }

  import Rank._
  import Suit._

  case class Card(rank: Rank, suit: Suit)

  sealed trait OneOrMoreHands

  final case class TexasHand private(cards: (Card, Card)) extends OneOrMoreHands
  object TexasHand {
    def create(cards: (Card, Card)): Option[TexasHand] =
      if (cards._1 != cards._2) Some(TexasHand(cards))
      else None
  }

  final case class OmahaHand private(cards: (Card, Card, Card, Card)) extends OneOrMoreHands
  object OmahaHand {
    def create(cards: (Card, Card, Card, Card)): Option[OmahaHand] =
      if (cards.productIterator.toSet.size == 4) Some(OmahaHand(cards))
      else None
  }

  // Borrowed type classes from the next lecture
  // to enforce that elements in Hands must one of the following types and all of the same type
  // Technically could have avoided this if I modeled the hands with lists instead of tuples,
  // but I prefer tuples here, since they demonstrate each hands arity better
  @annotation.implicitNotFound("${T} is not of type OmahaHand or TexasHand")
  sealed trait RestrictedHands[T]
  object RestrictedHands {
    implicit val omahaHands: RestrictedHands[OmahaHand] = new RestrictedHands[OmahaHand] {}
    implicit val texasHands: RestrictedHands[TexasHand] = new RestrictedHands[TexasHand] {}
  }

  final case class Hands[T: RestrictedHands] private(hands: List[T]) extends OneOrMoreHands
  object Hands {
    def create[T: RestrictedHands](hands: List[T]): Option[Hands[T]] =
      if (hands.toSet.size == hands.size) Some(Hands(hands))
      else None
  }

  final case class Board private(cards: (Card, Card, Card, Card, Card)) extends AnyVal

  object Board {
    def create(cards: (Card, Card, Card, Card, Card)): Option[Board] =
      if (cards.productIterator.toSet.size == 5) Some(Board(cards))
      else None
  }

  object Combination {
    final case class HighCard(card: Card)
    final case class Pair(cards: (Card, Card))
    final case class TwoPairs(pairs: ((Card, Card), (Card, Card)))
    final case class ThreeOfAKind(cards: (Card, Card, Card))
    final case class Straight(cards: (Card, Card, Card, Card, Card))
    final case class Flush(cards: (Card, Card, Card, Card, Card))
    final case class FullHouse(cards: (Card, Card, Card, Card, Card))
    final case class FourOfAKind(cards: (Card, Card, Card, Card))
    final case class StraightFlush(cards: (Card, Card, Card, Card, Card))
  }

  def rank[T: RestrictedHands](board: Board, hands: Hands[T]): List[OneOrMoreHands] = ???

  // Test case
  rank(
    Board((
      Card(Four, Clubs),
      Card(King, Spades),
      Card(Four, Hearts),
      Card(Eight, Spades),
      Card(Seven, Spades))),
    Hands(List(
      TexasHand((
        Card(Ace, Diamonds),
        Card(Four, Spades))),
      TexasHand((
        Card(Ace, Clubs),
        Card(Four, Diamonds))),
      TexasHand((
        Card(Ace, Spades),
        Card(Nine, Spades))),
      TexasHand((
        Card(King, Hearts),
        Card(King, Diamonds))),
      TexasHand((
        Card(Five, Diamonds),
        Card(Six, Diamonds))))))

  // Test result
  val res: List[OneOrMoreHands] = List(
    Hands(List(
      TexasHand((
        Card(Ace, Clubs),
        Card(Four, Diamonds))),
      TexasHand((
        Card(Ace, Diamonds),
        Card(Four, Spades))))),
    TexasHand((
      Card(Five, Diamonds),
      Card(Six, Diamonds))),
    TexasHand((
      Card(Ace, Spades),
      Card(Nine, Spades))),
    TexasHand((
      Card(King, Hearts),
      Card(King, Diamonds)))
  )

  // Works with Omaha hands
  rank(
    Board((
      Card(Four, Clubs),
      Card(King, Spades),
      Card(Four, Hearts),
      Card(Eight, Spades),
      Card(Seven, Spades))),
    Hands(List(
      OmahaHand((
        Card(Ace, Diamonds),
        Card(Four, Spades),
        Card(Ace, Clubs),
        Card(Four, Diamonds))),
      OmahaHand((
        Card(Five, Diamonds),
        Card(Six, Diamonds),
        Card(Ace, Spades),
        Card(Nine, Spades))))))

  // compiler complains about work with mixed hands thanks to the implicit type class
  /*
  rank(
    Board((
      Card(Four, Clubs),
      Card(King, Spades),
      Card(Four, Hearts),
      Card(Eight, Spades),
      Card(Seven, Spades))),
    Hands(List(
      OmahaHand((
        Card(Ace, Diamonds),
        Card(Four, Spades),
        Card(Ace, Clubs),
        Card(Four, Diamonds))),
      TexasHand((
        Card(Five, Diamonds),
        Card(Six, Diamonds))))))
   */
}
