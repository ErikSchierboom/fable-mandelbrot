module App

open Fable.Import

let window = Browser.Dom.window

let canvas: Browser.Types.HTMLCanvasElement = unbox window.document.querySelector "canvas"

let body = window.document.querySelector "body"

let maxWidth = canvas.parentElement.offsetWidth
let maxHeight = canvas.parentElement.offsetHeight
let targetWidthHeightRatio = 1.5
let actualWidthHeightRatio = maxWidth / maxHeight

let width =
    if actualWidthHeightRatio > targetWidthHeightRatio
    then round (maxHeight * targetWidthHeightRatio)
    else maxWidth

let height =
    if actualWidthHeightRatio > targetWidthHeightRatio
    then maxHeight
    else round (maxWidth / targetWidthHeightRatio)

canvas.width <- width
canvas.height <- height

let ctx = canvas.getContext_2d (Some({| alpha = false |}))

let xmin = -2.0
let xmax = 1.0
let ymin = -1.0
let ymax = 1.0

let maxIterations = 1000

let mandelIterations cx cy =
    let mutable x = 0.0
    let mutable y = 0.0
    let mutable xx = 0.0
    let mutable yy = 0.0
    let mutable xy = 0.0

    let mutable i = 1

    while i <= maxIterations && xx + yy <= 4.0 do
        xy <- x * y
        xx <- x * x
        yy <- y * y
        x <- xx - yy + cx
        y <- xy + xy + cy
        i <- i + 1

    i

let mandelColor i =
    if i > maxIterations then
        (0uy, 0uy, 0uy, 255uy)
    else
        let c = (3.0 * System.Math.Log(float i)) / System.Math.Log(float maxIterations - 1.0)

        if c < 1.0
        then (byte (255.0 * c), 0uy, 0uy, 255uy)
        elif c < 2.0
        then (255uy, byte (255.0 * (c - 1.0)), 0uy, 255uy)
        else (255uy, 255uy, byte (255.0 * (c - 2.0)), 255uy)

let img = ctx.getImageData (0.0, 0.0, width, height)
let data = img.data

let xdiff = xmax - xmin
let ydiff = ymax - ymin
let xdiv = width - 1.0
let ydiv = height - 1.0

for iy in 0.0 .. height do
    for ix in 0.0 .. width do

        let x = xmin + (xdiff * ix) / xdiv
        let y = ymin + (ydiff * iy) / ydiv

        let iterations = mandelIterations x y

        let ppos = 4 * int (width * iy + ix)
        let (r, g, b, a) = mandelColor iterations

        data.[ppos] <- r
        data.[ppos + 1] <- g
        data.[ppos + 2] <- b
        data.[ppos + 3] <- a

ctx.putImageData (img, 0.0, 0.0)
