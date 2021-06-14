
{--                 INSTRUCTIONS

  Given an age in seconds, calculate how old someone would be on:

  Mercury: orbital period 0.2408467 Earth years
  Venus:   orbital period 0.61519726 Earth years
  Earth:   orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
  Mars:    orbital period 1.8808158 Earth years
  Jupiter: orbital period 11.862615 Earth years
  Saturn:  orbital period 29.447498 Earth years
  Uranus:  orbital period 84.016846 Earth years
  Neptune: orbital period 164.79132 Earth years

  So if you were told someone were 1,000,000,000 seconds old, you should be able 
  to say that they're 31.69 Earth-years old.

  If you're wondering why Pluto didn't make the cut, go watch this youtube video.

  Hints
  In this exercise, we provided the definition of the algebric data type named Planet. 
  You need to implement the ageOn function, that calculates how many years old someone 
  would be on a Planet, given an age in seconds.

  Your can use the provided signature if you are unsure about the types, 
  but don't let it restrict your creativity:

  ageOn :: Planet -> Float -> Float

--}


module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet sec = sec / ageInSecOnEarth / orbitalPeriod planet             
    where
      ageInSecOnEarth = 31557600
      
      orbitalPeriod :: Planet -> Float
      orbitalPeriod Mercury = 0.2408467
      orbitalPeriod Venus   = 0.61519726
      orbitalPeriod Earth   = 1.0
      orbitalPeriod Mars    = 1.8808158
      orbitalPeriod Jupiter = 11.862615
      orbitalPeriod Saturn  = 29.447498
      orbitalPeriod Uranus  = 84.016846
      orbitalPeriod Neptune = 164.79132


