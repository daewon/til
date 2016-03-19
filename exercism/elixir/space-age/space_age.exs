defmodule SpaceAge do
  @planet [mercury: 0.2408467,
           venus: 0.61519726,
           earth: 1,
           mars: 1.8808158,
           jupiter: 11.862615,
           saturn: 29.447498,
           neptune: 164.79132,
           uranus: 84.016846]

  @type planet :: :mercury | :venus | :earth | :mars | :jupiter
  | :saturn | :neptune | :uranus
  @doc """
  Return the number of years a person that has lived for 'seconds' seconds is
  aged on 'planet'.
  """
  @spec age_on(planet, pos_integer) :: float
  def age_on(planet, seconds) do
    seconds / 31557600 / @planet[planet]
  end

  # - Earth: orbital period 365.25 Earth days, or 31557600 seconds
  # - Mercury: orbital period 0.2408467 Earth years
  # - Venus: orbital period 0.61519726 Earth years
  # - Mars: orbital period 1.8808158 Earth years
  # - Jupiter: orbital period 11.862615 Earth years
  # - Saturn: orbital period 29.447498 Earth years
  # - Uranus: orbital period 84.016846 Earth years
  # - Neptune: orbital period 164.79132 Earth years

  # So if you were told someone were 1,000,000,000 seconds old, you should
  # be able to say that they're 31 Earth-years old.
end
