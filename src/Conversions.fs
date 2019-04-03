module Twitcher.Conversions

open Twitcher.Domain

module Speed =
  let private msfmConst = 196.8504<(ft/minute)/(m/s)>
  let ms2fm (speed: float<m/s>) = speed * msfmConst
  let fm2ms (speed: float<ft/minute>) = speed/msfmConst

  let private msknotConst = 1.94384<knot/(m/s)>
  let ms2knot (speed: float<m/s>) = speed * msknotConst
  let knot2ms (speed: float<knot>) = speed/msknotConst

  let private machknotConst = 666.738661<knot/Mach>
  let mach2knot (speed: float<Mach>) = speed * machknotConst
  let knot2mach (speed: float<knot>) = speed/machknotConst

  let private kmhknotConst =  0.539957<knot/(km/h)>
  let kmh2knot (speed: float<km/h>) = speed * kmhknotConst  
  let knot2kmh (speed: float<knot>) = speed/kmhknotConst  

module Altitude =
  /// Conversion constant between feet and meters
  let private mftConst = 3.2808<ft/m>  
  let m2ft (alt: float<m>) = alt * mftConst
  let ft2m (alt: float<ft>) = alt/mftConst

  let private ftflConst = 100.<ft/FL>
  let ft2fl (alt: float<ft>) = (alt/ftflConst) |> float |> round |> int |> (*) 1<FL>
  let fl2ft (alt: int<FL>) = ((float alt) * 1.<FL>)*ftflConst

module Distance =
  let private mnmConst = 1852.<m/nm>
  let m2nm (d: float<m>) = d/mnmConst 
  let nm2m (d: float<nm>) = d*mnmConst