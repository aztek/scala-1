object ZebraPuzzle {
  object House extends Enumeration {
    type House = Value
    val First, Second, Third, Fourth, Fifth = Value
  }
  import House._

  object Color extends Enumeration {
    type Color = Value
    val Red, Green, Yellow, Ivory, Blue = Value
  }
  import Color._

  object Nationality extends Enumeration {
    type Nationality = Value
    val English, Spanish, Ukrainian, Norwegian, Japanese = Value
  }
  import Nationality._

  object Pet extends Enumeration {
    type Pet = Value
    val Dog, Snail, Horse, Fox, Zebra = Value
  }
  import Pet._

  object Beverage extends Enumeration {
    type Beverage = Value
    val Coffee, Tea, Milk, OrangeJuice, Water = Value
  }
  import Beverage._

  object Brand extends Enumeration {
    type Brand = Value
    val OldGold, Kools, Chesterfield, LuckyStrike, Parliament = Value
  }
  import Brand._

  final case class Resident(house: House, color: Color, nationality: Nationality,
                            pet: Pet, beverage: Beverage, brand: Brand) {
    def toTheRightTo(r2: Resident): Boolean =
      (r2.house == First  && house == Second) ||
        (r2.house == Second && house == Third)  ||
        (r2.house == Third  && house == Fourth) ||
        (r2.house == Fourth && house == Fifth)

    def nextTo(r: Resident): Boolean = toTheRightTo(r) || r.toTheRightTo(this)
  }

  sealed trait Constraint extends Product with Serializable
  final case class Single(c: Resident => Boolean) extends Constraint
  final case class Pair(c: (Resident, Resident) => Boolean) extends Constraint

  def satisfies(r: Resident): Boolean = specification forall {
    case Single(constraint) => constraint(r)
    case Pair(_) => true
  }

  def satisfy(r1: Resident, r2: Resident): Boolean = specification forall {
    case Single(_) => true
    case Pair(constraint) => constraint(r1, r2)
  }

  val specification: List[Constraint] = List(
    // 1. There are five houses.

    // 2. The Englishman lives in the red house.
    Single(r => (r.nationality == English) == (r.color == Red)),

    // 3. The Spaniard owns the dog.
    Single(r => (r.nationality == Spanish) == (r.pet == Dog)),

    // 4. Coffee is drunk in the green house.
    Single(r => (r.beverage == Coffee) == (r.color == Green)),

    // 5. The Ukrainian drinks tea.
    Single(r => (r.nationality == Ukrainian) == (r.beverage == Tea)),

    // 6. The green house is immediately to the right of the ivory house.
    Pair((r1, r2) => (r1 toTheRightTo r2) == (r1.color == Green && r2.color == Ivory)),

    // 7. The Old Gold smoker owns snails.
    Single(r => (r.brand == OldGold) == (r.pet == Snail)),

    // 8. Kools are smoked in the yellow house.
    Single(r => (r.brand == Kools) == (r.color == Yellow)),

    // 9. Milk is drunk in the middle house.
    Single(r => (r.beverage == Milk) == (r.house == Third)),

    // 10. The Norwegian lives in the first house.
    Single(r => (r.nationality == Norwegian) == (r.house == First)),

    // 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
    Pair((r1, r2) => !(r1.brand == Chesterfield && r2.pet == Fox) || (r1 nextTo r2)),

    // 12. Kools are smoked in the house next to the house where the horse is kept.
    Pair((r1, r2) => !(r1.brand == Kools && r2.pet == Horse) || (r1 nextTo r2)),

    // 13. The Lucky Strike smoker drinks orange juice.
    Single(r => (r.brand == LuckyStrike) == (r.beverage == OrangeJuice)),

    // 14. The Japanese smokes Parliaments.
    Single(r => (r.nationality == Japanese) == (r.brand == Parliament)),

    // 15. The Norwegian lives next to the blue house.
    Pair((r1, r2) => !(r1.nationality == Norwegian && r2.color == Blue) || (r1 nextTo r2))
  )

  def uniqueResident(residents: List[Resident]): Stream[Resident] = {
    // unzip6? :(
    val houses = residents map (_.house)
    val colors = residents map (_.color)
    val nationalities = residents map (_.nationality)
    val pets = residents map (_.pet)
    val beverages = residents map (_.beverage)
    val brands = residents map (_.brand)
    for {
      house       <- House.values.diff(houses.toSet).toStream
      color       <- Color.values.diff(colors.toSet).toStream
      nationality <- Nationality.values.diff(nationalities.toSet).toStream
      pet         <- Pet.values.diff(pets.toSet).toStream
      beverage    <- Beverage.values.diff(beverages.toSet).toStream
      brand       <- Brand.values.diff(brands.toSet).toStream
      resident = Resident(house, color, nationality, pet, beverage, brand)
      if satisfies(resident) && residents.forall(satisfy(_, resident))
    } yield resident
  }

  def solutions(count: Int): Stream[List[Resident]] = for {
    rs <- if (count > 1) solutions(count - 1)
          else Stream(Nil)
    r <- uniqueResident(rs)
  } yield r :: rs

  case class Solution(waterDrinker: Nationality, zebraOwner: Nationality)

  lazy val solve: Solution = {
    val solution = solutions(5).head
    val waterDrinker = solution.find(_.beverage == Water).get.nationality
    val zebraOwner = solution.find(_.pet == Zebra).get.nationality
    Solution(waterDrinker, zebraOwner)
  }
}

