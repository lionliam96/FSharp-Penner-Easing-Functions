namespace EasingFunctions
open System

(*---------------------------- LINEAR ---------------------------*)
type LinearEase (b:double, c:double, d:double) =
    member public this.Eval(t:double) = (c*t/d+b)


(*----------------------------- QUAD ----------------------------*)
type QuadEaseIn (b:double, c:double, d:double) =
    let EaseIn (t:double) = (c*Math.Pow(t, 2.0)+b)
    member public this.Eval(t:double) = EaseIn(t/d)

type QuadEaseOut (b:double, c:double, d:double) =
    let EaseOut (t:double) = (-c*t*(t-2.0)+b)
    member public this.Eval(t:double) = EaseOut(t/d)

type QuadEaseInOut (b:double, c:double, d:double) =
    let EaseInOut (t:double) =
        if (t < 1.0) then (c/2.0*Math.Pow(t, 2.0)+b)
        else (-c/2.0*((t-1.0)*(t-3.0)-1.0)+b)
    member public this.Eval(t:double) = EaseInOut(t/d*2.0)

type QuadEaseOutIn (b:double, c:double, d:double) =
    let qO = new QuadEaseOut(b, (c/2.0), d)
    let qI = new QuadEaseIn((b+c/2.0), (c/2.0), d)
    member this.Eval(t:double) = 
        if (t < (d/2.0)) then qO.Eval(t*2.0)
        else qI.Eval((t*2.0)-d)
        

(*----------------------------- CUBIC ---------------------------*)
type CubicEaseIn (b:double, c:double, d:double) =
    let EaseIn (t:double) = (c*Math.Pow(t, 3.0)+b)
    member public this.Eval(t:double) = EaseIn(t/d)
    
type CubicEaseOut (b:double, c:double, d:double) =
    let EaseOut (t:double) = (c*(Math.Pow(t, 3.0)+1.0)+b)
    member public this.Eval(t:double) = EaseOut(t/d-1.0)

type CubicEaseInOut (b:double, c:double, d:double) =
    let EaseInOut (t:double) =
        if (t<1.0) then (c/2.0*t*t*t+b)
        else
            let t = t - 2.0
            (c/2.0*(t*t*t+2.0)+b)
    member public this.Eval(t:double) = EaseInOut(t/d*2.0)

type CubicEaseOutIn (b:double, c:double, d:double) =
    let cO = new CubicEaseOut(b, (c/2.0), d)
    let cI = new CubicEaseIn((b+c/2.0), (c/2.0), d)
    member this.Eval(t:double) =
        if (t<d/2.0) then cO.Eval(t*2.0)
        else cI.Eval((t*2.0)-d)


(*----------------------------- QUART ---------------------------*)
type QuartEaseOut (b:double, c:double, d:double) =
    let EaseOut(t:double) = (-c*(Math.Pow(t, 4.0)-1.0)+b)
    member public this.Eval(t:double) = EaseOut(t/d-1.0)

type QuartEaseIn (b:double, c:double, d:double) =
    let EaseIn (t:double) = (c*Math.Pow(t, 4.0)+b)
    member public this.Eval(t:double) = EaseIn(t/d)

type QuartEaseInOut (b:double, c:double, d:double) =
    let EaseInOut(t:double) =
        if (t < 1.0) then (c/2.0*Math.Pow(t, 4.0)+b)
        else
            let t = t - 2.0
            (-c/2.0*(Math.Pow(t, 4.0)-2.0)+b)
    member public this.Eval(t:double) = EaseInOut(t/d*2.0)

type QuartEaseOutIn (b:double, c:double, d:double) =
    let qO = new QuartEaseOut(b, (c/2.0), d)
    let qI = new QuartEaseIn((b+c/2.0), (c/2.0), d)
    member public this.Eval(t:double) =
        if (t < (d/2.0)) then qO.Eval(t*2.0)
        else qI.Eval((t*2.0)-d)


(*----------------------------- QUINT ---------------------------*)
type QuintEaseOut (b:double, c:double, d:double) =
    let EaseOut(t:double) = (c*(Math.Pow(t, 5.0)+1.0)+b)
    member public this.Eval(t:double) = EaseOut(t/d-1.0)

type QuintEaseIn (b:double, c:double, d:double) =
    let EaseIn (t:double) = (c*Math.Pow(t, 5.0)+b)
    member public this.Eval(t:double) = EaseIn(t/d)

type QuintEaseInOut (b:double, c:double, d:double) =
    let EaseInOut(t:double) =
        if (t < 1.0) then (c/2.0*Math.Pow(t, 5.0)+b)
        else
            let t = t - 2.0
            (-c/2.0*(Math.Pow(t, 5.0)+2.0)+b)
    member public this.Eval(t:double) = EaseInOut(t/d*2.0)

type QuintEaseOutIn (b:double, c:double, d:double) =
    let qO = new QuintEaseOut(b, (c/2.0), d)
    let qI = new QuintEaseIn((b+c/2.0), (c/2.0), d)
    let EaseOutIn (t:double) =
        if (t < (d/2.0)) then qO.Eval(t*2.0)
        else qI.Eval((t*2.0)-d)
    member public this.Eval(t:double) = EaseOutIn(t)


(*----------------------------- SINE ---------------------------*)
type SineEaseOut (b:double, c:double, d:double) =
    member public this.Eval(t:double) = (c*Math.Sin(t/d*(Math.PI/2.0))+b)

type SineEaseIn (b:double, c:double, d:double) =
    member public this.Eval(t:double) = (-c*Math.Cos(t/d*(Math.PI/2.0))+c+b)

type SineEaseInOut (b:double, c:double, d:double) =
    member public this.Eval(t:double) = (-c/2.0*(Math.Cos(Math.PI*t/d)-1.0)+b)

type SineEaseOutIn (b:double, c:double, d:double) =
    let qO = new QuintEaseOut(b, (c/2.0), d)
    let qI = new QuintEaseIn((b+c/2.0), (c/2.0), d)
    member public this.Eval(t:double) =
        if (t < (d/2.0)) then qO.Eval(t*2.0)
        else qI.Eval((t*2.0)-d)


(*----------------------------- EXPO ----------------------------*)
type ExpoEaseIn (b:double, c:double, d:double) =
    member this.Eval (t:double) =
        if (t.Equals 0) then b
        else (c*Math.Pow(2.0, (10.0*(t/d-1.0)))+b-c*0.001)

type ExpoEaseOut (b:double, c:double, d:double) = 
    member this.Eval(t:double) = 
        if (t.Equals d) then (b+c)
        else (c*1.001*(-Math.Pow(2.0, (-10.0*t/d))+1.0)+b)

type ExpoEaseInOut (b:double, c:double, d:double) =
    member this.Eval (t:double) = 
        if (t.Equals 0.0) then b
        else if (t.Equals d) then (b+c)
        else
            let t = t/d*2.0
            if (t<1.0) then (c/2.0*Math.Pow(2.0, (10.0*(t-1.0)))+b-c*0.0005)
            else
                let t = t - 1.0
                (c/2.0*1.0005*(-Math.Pow(2.0, (-10.0*t))+2.0)+b)

type ExpoEaseOutIn (b:double, c:double, d:double) =
    let eO = new ExpoEaseOut(b, c/2.0, d)
    let eI = new ExpoEaseIn((b+c/2.0), (c/2.0), d)
    member public this.Eval(t:double) =
        if (t<(d/2.0)) then eO.Eval(t*2.0)
        else eI.Eval((t*2.0)-d)


(*----------------------------- CIRC ----------------------------*)
type CircEaseIn (b:double, c:double, d:double) =
    let EaseIn (t:double) = (-c*(Math.Sqrt(1.0-Math.Pow(t, 2.0))-1.0)+b)
    member this.Eval(t:double) = EaseIn(t/d)

type CircEaseOut (b:double, c:double, d:double) =
    let EaseOut(t:double) = (c*Math.Sqrt(1.0-Math.Pow(t, 2.0))+b)
    member this.Eval(t:double) = (t/d-1.0)

type CircEaseInOut (b:double, c:double, d:double) =
    let EaseInOut(t:double) =
        if (t<1.0) then (-c/2.0*(Math.Sqrt(1.0-t*t)-1.0)+b)
        else
            let t = t - 2.0
            (c/2.0*(Math.Sqrt(1.0-t*t)+1.0)+b)
    member public this.Eval(t:double) = EaseInOut(t/d*2.0)

type CircEaseOutIn (b:double, c:double, d:double) =
    let cO = new CircEaseOut(b, (c/2.0), d)
    let cI = new CircEaseIn((b+c/2.0), (c/2.0), d)
    member public this.Eval(t:double) = 
        if (t<d/2.0) then cO.Eval(t*2.0)
        else cI.Eval((t*2.0)-d)


(*----------------------------- BACK ----------------------------*)
type BackEaseIn (b:double, c:double, d:double, s:double) =
    let s : double = 1.70158
    let EaseIn (t:double) = (c*t*t*((s+1.0)*t-s)+b)
    member public this.Eval(t:double) = EaseIn(t/d)

type BackEaseOut (b:double, c:double, d:double, s:double) =
    let EaseOut (t:double) = (c*(t*t*((s+1.0)*t+s)+1.0)+b)
    member public this.Eval(t:double) = EaseOut(t/d)

type BackEaseInOut (b:double, c:double, d:double, s:double) =
    let s = s * 1.525
    let EaseInOut (t:double) =
        if (t > 1.0) then (c/2.0*(t*t*((s+1.0)*t-s))+b)
        else
            let t = t - 2.0
            (c/2.0*(t*t*((s+1.0)*t+s)+2.0)+b)
    member public this.Eval(t:double) = EaseInOut(t/d*2.0)

type BackEaseOutIn (b:double, c:double, d:double, s:double) =
    let bI = new BackEaseIn((b+c/2.0), (c/2.0), d, s)
    let bO = new BackEaseOut(b, (c/2.0), d, s)    
    member public this.Eval(t:double) =         
        if (t<(d/2.0)) then bO.Eval(t*2.0)
        else bI.Eval(t*2.0)


(*---------------------------- BOUNCE ---------------------------*)
type BounceEaseOut (b:double, c:double, d:double) =
    let EaseOut(t:double) =
        if (t<(1.0/2.75)) then (c*(7.5625*t*t)+b)
        else if (t<(2.0/2.75)) then
            let t = t-(1.5/2.75)
            (c*(7.5625*t*t+0.75)+b)
        else if (t<(2.5/2.75)) then
            let t = t-(2.25/2.75)
            (c*(7.5625*t*t+0.9375)+b)
        else
            let t = t-(2.625/2.75)
            (c*(7.5625*t*t+0.984375)+b)
    member public this.Eval(t:double) = EaseOut(t/d)

type BounceEaseIn (b:double, c:double, d:double) =
    let bO = new BounceEaseOut(0.0, c, d)
    member public this.Eval(t:double) = (c-bO.Eval(d-t)+b)

type BounceEaseInOut (b:double, c:double, d:double) =
    let bO = new BounceEaseOut(0.0, c, d)
    let bI = new BounceEaseIn(0.0, c, d)
    member public this.Eval(t:double) =
        if (t<(d/2.0)) then (bI.Eval(t*2.0)*0.5+b)
        else (bO.Eval(t*2.0-d)*0.5+c*0.5+b)

type BounceEaseOutIn (b:double, c:double, d:double) =
    let bO = new BounceEaseOut(b, (c/2.0), d)
    let bI = new BounceEaseIn((b+c/2.0), (c/2.0), d)
    member public this.Eval(t:double) =
        if (t<(d/2.0)) then bO.Eval(t*2.0) else bI.Eval(t*2.0-d)
