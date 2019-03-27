module Twitcher.Conversions

open Twitcher.Domain

module Speed =
  let msfmConst = 196.8504<(ft/minute)/(m/s)>

  let ms2fm (speed: float<m/s>) =
    speed * msfmConst

  let fm2ms (speed: float<ft/minute>) =
    speed/msfmConst

  let msknotConst = 1.94384<knot/(m/s)>
  let ms2knot (speed: float<m/s>) = speed * msknotConst
  let knot2ms (speed: float<knot>) = speed/msknotConst

module Altitude =
  /// Conversion constant between feet and meters
  let mftConst = 3.2808<ft/m>  
  let m2ft (alt: float<m>) = alt * mftConst
  let ft2m (alt: float<ft>) = alt/mftConst

  