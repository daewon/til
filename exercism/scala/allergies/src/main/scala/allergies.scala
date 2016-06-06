object Allergen  {
  sealed trait AllergenBase { val value: Int }

  case object Eggs extends AllergenBase { override val value = 1 }
  case object Peanuts extends AllergenBase { override val value = 2 }
  case object Shellfish extends AllergenBase { override val value = 4 }
  case object Strawberries extends AllergenBase { override val value = 8 }
  case object Tomatoes extends AllergenBase { override val value = 16 }
  case object Chocolate extends AllergenBase { override val value = 32 }
  case object Pollen extends AllergenBase { override val value = 64 }
  case object Cats extends AllergenBase { override val value = 128 }

  val allengens = Seq(Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats)
}

case class Allergies() {
  import Allergen._

  def isAllergicTo(allerge: AllergenBase, n: Int): Boolean = allerge.value <= n

  def allergies(n: Int): Seq[AllergenBase] = {
    def find(remain: Int, ls: Seq[AllergenBase], acc: Set[AllergenBase] = Set.empty): Seq[AllergenBase] = ls match {
      case Nil => acc.toList.sortBy(_.value)
      case hd :: tl =>
        if (isAllergicTo(hd, remain)) find(remain - hd.value, ls, acc + hd)
        else find(remain, tl, acc)
    }

    find(n, allengens.reverse)
  }
}
