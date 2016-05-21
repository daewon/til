/*
 Write a program that, given an seconds in seconds, calculates how old someone is in terms of a given planet's solar years.

 Given an seconds in seconds, calculate how old someone would be on:

 - Earth: orbital period 365.25 Earth days, or 31557600 seconds
 - Mercury: orbital period 0.2408467 Earth years
 - Venus: orbital period 0.61519726 Earth years
 - Mars: orbital period 1.8808158 Earth years
 - Jupiter: orbital period 11.862615 Earth years
 - Saturn: orbital period 29.447498 Earth years
 - Uranus: orbital period 84.016846 Earth years
 - Neptune: orbital period 164.79132 Earth years

 So if you were told someone were 1,000,000,000 seconds old, you should
 be able to say that they're 31 Earth-years old.
 */

object SpaceAge {
  def apply(seconds: Long) = new SpaceAge(seconds)
}

class SpaceAge(val seconds: Double) {
  val earthSeconds = seconds / 31557600

  def calc(period: Double): Double =
    Math.round((earthSeconds / period) * 100).toDouble / 100

  def onEarth: Double = calc(1)
  def onMercury: Double = calc(0.2408467)
  def onVenus: Double = calc(0.61519726)
  def onMars: Double = calc(1.8808158)
  def onJupiter: Double = calc(11.862615)
  def onSaturn: Double = calc(29.447498)
  def onUranus: Double = calc(84.016846)
  def onNeptune: Double = calc(164.79132)
}
