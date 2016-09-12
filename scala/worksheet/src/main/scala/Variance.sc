trait Drink

trait SoftDrink extends Drink

trait Juice extends Drink

case class Cola() extends SoftDrink

case class TonicWater() extends SoftDrink

case class OrangeJuice() extends Juice

case class AppleJuice() extends Juice


class VendingMachine[+A]

//def install(softDrinkVM: VendingMachine[SoftDrink]): Unit = {}
//
//
//// covariant
//install(new VendingMachine[Cola])
//install(new VendingMachine[TonicWater])
//
//// invariant
//install(new VendingMachine[SoftDrink])
//
//// compile error! vending machine can take Covariant
//// install(new VendingMachine[Drink])


trait Bullet

trait NormalBullet extends Bullet

trait Note7Bullet extends Bullet

class AmmoMagazine[+A <: Bullet](private[this] var bullets: List[A]) {
  def hasBullets: Boolean = bullets.nonEmpty

  def giveNextBullets(): Option[A] = {
    bullets match {
      case Nil => None
      case x :: xs =>
        bullets = xs
        Some(x)
    }

  }
}

class Gun(private var ammo: AmmoMagazine[Bullet]) {
  def reload(ammo: AmmoMagazine[Bullet]): Unit = {
    this.ammo = ammo
  }

  def hasAmmo: Boolean = ammo.hasBullets

  def shoot: Option[Bullet] = ammo.giveNextBullets()
}


class GarbageCan[-A] {

  // compiles because of object private scope
  private[this] var items: List[A] = List.empty

  def put(item: A): Unit = this.items :+= item

  def putAll(items: List[A]): Unit = this.items ++= items

  def itemsCount: Int = this.items.size

}

trait Read[+A] {
  def read(s: String): Option[A]
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

val listFunctor = new Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
}

trait Base
class Shape() extends Base
class Circle() extends Shape

val xsCircle = List(new Circle(), new Circle())
listFunctor.map(xsCircle) { c => c: Shape }

def upcast[F[_], A, B <: A](functor: Functor[F], fb: F[B]): F[A] =
  functor.map(fb)(b => b: A)

upcast[List, Base, Shape](listFunctor, xsCircle)

val fn = (a: Shape) => a.toString
val fnBase: Circle => String = fn

