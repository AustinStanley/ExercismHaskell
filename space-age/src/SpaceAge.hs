module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

getRatio :: Planet -> Float
getRatio Mercury = 0.2408467
getRatio Venus   = 0.61519726
getRatio Earth   = 1
getRatio Mars    = 1.8808158
getRatio Jupiter = 11.862615
getRatio Saturn  = 29.447498
getRatio Uranus  = 84.016846
getRatio Neptune = 164.79132

earthYears :: Fractional a => a -> a
earthYears = (/ secondsPerYear)
    where secondsPerYear = 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = (earthYears seconds) / (getRatio planet)
