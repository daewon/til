// http://egloos.zum.com/ani2life/v/2887675

// abstract factory

trait UnitBase {
  def attack: String
}

class Marine extends UnitBase {
  def attack = "Marine Attack"
}

class Medic extends UnitBase {
  def attack = "Medic Attack"
}

class Battle extends UnitBase {
  def attack = "Battle Attack"
}

class Wraith extends UnitBase {
  def attack = "Wraith Attack"
}

trait AbstractFactory {
  def create(tpe: String): UnitBase
}

class Barracks extends AbstractFactory {
  override def create(tpe: String): UnitBase = tpe match {
    case "Marine" => new Marine
    case "Medic" => new Medic
  }
}

class StarPort extends AbstractFactory {
  override def create(tpe: String): UnitBase = tpe match {
    case "Battle" => new Battle
    case "Wraith" => new Wraith
  }
}
