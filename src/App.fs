module App

open Fable.Core.JsInterop
open Fable.Import
open Expr


open Fable.Core

type LZString = 
  abstract compressToBase64 : string -> string
  abstract decompressFromBase64 : string -> string

[<ImportAll("lz-string")>]
let LZMA: LZString = jsNative


let window = Browser.Dom.window

// Get our canvas context 
// As we'll see later, myCanvas is mutable hence the use of the mutable keyword
// the unbox keyword allows to make an unsafe cast. Here we assume that getElementById will return an HTMLCanvasElement 
let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // myCanvas is defined in public/index.html
let mutable myButton : Browser.Types.HTMLInputElement = unbox window.document.getElementById "go"  // myCanvas is defined in public/index.html
let mutable myResults : Browser.Types.HTMLPreElement = unbox window.document.getElementById "results"  // myCanvas is defined in public/index.html
let myCanvasBuffer : Browser.Types.HTMLCanvasElement = unbox window.document.createElement "canvas"
let myCanvasBufferImage : Browser.Types.HTMLCanvasElement = unbox window.document.createElement "canvas"
let interpretationError : Browser.Types.HTMLDivElement = unbox window.document.getElementById "interpretation-error"

let showError () = 
  interpretationError?style?removeProperty "display"
  myButton.classList.add "btn-danger"

let hideError () = 
  interpretationError?style?setProperty ("display", "none")
  myButton.classList.add "btn-secondary"
  myButton.classList.remove "btn-primary"
  myButton.classList.remove "btn-success"
  myButton.classList.remove "btn-danger"

// Get the context
let ctx = myCanvas.getContext_2d()

// All these are immutables values
let w = myCanvas.width
let h = myCanvas.height
let ratio = window.devicePixelRatio
let steps = 20
let squareSize = 20

// gridWidth needs a float wo we cast tour int operation to a float using the float keyword
let gridWidth = float (steps * squareSize) 

// resize our canvas to the size of our grid
// the arrow <- indicates we're mutating a value. It's a special operator in F#.
myCanvas.width <- gridWidth
myCanvas.height <- gridWidth
myCanvas.width <- w * ratio
myCanvas.height <- h * ratio
ctx.scale(ratio, ratio)

myCanvas?style?height <- sprintf "%dpx" (int h)
myCanvas?style?width <- sprintf "%dpx" (int w)

myCanvasBuffer.width <- w * ratio
myCanvasBuffer.height <- h * ratio
myCanvasBuffer?style?height <- sprintf "%dpx" (int h)
myCanvasBuffer?style?width <- sprintf "%dpx" (int w)

type PenState = Up | Down | Erase
type Env = { X : float; Y : float; Pen : PenState; IsVisible : bool; Procedures : Map<string, ProcedureDef>; Angle : int16; BackgroudColor : Color; PenColor : Color; PenSize : uint16; Variables : Map<string, NumberExpr> }
let empty = { X = 0.; Y = 0.; Pen = Down; IsVisible = true; Procedures = Map.empty; Angle = 180s; BackgroudColor = Blue; PenColor = Black; PenSize = 3us; Variables = Map.empty }

let rec numExpr env = function
  | Add (a, b) -> (numExpr env a) + (numExpr env b)
  | Sub (a, b) -> (numExpr env a) - (numExpr env b)
  | Div (a, b) -> (numExpr env a) / (numExpr env b)
  | Mul (a, b) -> (numExpr env a) * (numExpr env b)
  | Num a -> a
  | Group a -> numExpr env a
  | Variable name ->
    match Map.tryFind name env with
    | Some x -> numExpr env x
    | None -> failwithf "Undefined variable '%A' in %A" name env

let rec numExprEval env = function
  | Add (a, b) -> Add(numExprEval env a, numExprEval env  b)
  | Sub (a, b) -> Sub(numExprEval env a, numExprEval env b)
  | Div (a, b) -> Div(numExprEval env a, numExprEval env  b)
  | Mul (a, b) -> Mul(numExprEval env a, numExprEval env b)
  | Group a -> Group(numExprEval env a)
  | Num a -> Num a
  | Variable name ->
    match Map.tryFind name env with
    | Some x -> numExpr env x |> Num
    | None -> Variable name

open System

let radConv = Math.PI / 180.

let drawTurtle env =
    let ctx = myCanvas.getContext_2d()
    let image:Browser.Types.HTMLImageElement = window?turtleImage
    
    myCanvasBufferImage.width <- 32.
    myCanvasBufferImage.height <- 32.
    
    let imgCtx = myCanvasBufferImage.getContext_2d()
    imgCtx.clearRect(0.,0., 32., 32.)
    imgCtx.save()
    imgCtx.translate(16.,16.)
    imgCtx.rotate (float (180s-env.Angle) * radConv)
    imgCtx.drawImage(U3.Case1 image,-16.,-16.,32., 32.)
    imgCtx.restore()
    ctx.drawImage(U3.Case2 myCanvasBufferImage, env.X / ratio - 16., env.Y / ratio - 16., 32., 32.)

let inline drawContext (ctx:Browser.Types.CanvasRenderingContext2D) env  f =
    ctx.beginPath ()
    ctx.lineWidth <- float env.PenSize
    ctx.fillStyle <- U3.Case1 env.BackgroudColor.Name
    ctx.strokeStyle <- U3.Case1 env.PenColor.Name
    f ctx
    ctx.stroke ()
    ctx.closePath ()

type Stack = { arr : int ResizeArray; mutable count : int }
with 
  member a.IsEmpty () = a.count = 0
  member a.Dequeue () =
    let index = a.count - 1
    if index >= 0 then
      a.count <- index
      a.arr.[index]
    else failwithf "Queue is empty"
  member a.Enqueue value = 
    if a.count = a.arr.Count then
      a.arr.Add value |> ignore
    else
      a.arr.[a.count] <- value
    a.count <- a.count + 1
      
  static member empty = { arr = ResizeArray (); count = 0 }

type Async<'a> =
  static member Singleton x = async { return x }

[<LiteralAttribute>]
let xMove = 4

let fill x y width height (arr:uint8 []) r g b =
  let pos x y = (y * width + x) * 4
  let r', g', b', a' = arr.[pos x y], arr.[pos x y + 1], arr.[pos x y + 2], arr.[pos x y + 3]

  
  let yMove = width * 4

  if (r <> r' || g <> g' || b <> b' || 255uy <> a') && x > 0 && y > 0  && x < height && y < width then
    let p = pos x y
    let pixels = Stack.empty
    pixels.Enqueue p |> ignore

    while pixels.IsEmpty () |> not do
      let pos = pixels.Dequeue ()

      if pos + 3 >= arr.Length || pos < 0 then ()
      elif arr.[pos] = r && arr.[pos + 1] = g && arr.[pos + 2] = b && arr.[pos + 3] = 255uy then ()
      elif (arr.[pos] = r' && arr.[pos + 1] = g' && arr.[pos + 2] = b' && arr.[pos + 3] = a')
        || (abs(int arr.[pos] - int r') < 25 && abs(int arr.[pos + 1] - int g') < 25 && abs(int arr.[pos + 2] - int b') < 25 && abs(int arr.[pos + 3] - int a') < 25) then
        arr.[pos] <- r
        arr.[pos + 1] <- g
        arr.[pos + 2] <- b
        arr.[pos + 3] <- 255uy

        let x = (pos / 4) % width

        if x < height - 1 then
          pixels.Enqueue(pos - yMove + xMove)
          pixels.Enqueue(pos + xMove)
          pixels.Enqueue(pos + yMove + xMove)

        if x > 0 then
          pixels.Enqueue(pos - xMove)
          pixels.Enqueue(pos - yMove - xMove)
          pixels.Enqueue(pos + yMove - xMove)
          
          
        pixels.Enqueue(pos - yMove)
        pixels.Enqueue(pos + yMove)
        



let exec (myCanvas : Browser.Types.HTMLCanvasElement) (expr:Expr list) =

  let ctx = myCanvasBuffer.getContext_2d()

  
  let rec exec (env:Env) xs = async {
    let (|Num|) = numExpr env.Variables
    match xs with 
    | [] -> 
      ctx.stroke()
      ctx.closePath()
      return env
    | Stop::_ -> return env
    | x::xs -> 
        let! env= 
          match x with
          | Stop -> failwithf "Impossible state - 'STOP' should be already handled"
          | If _ -> Async.Singleton env //todo: impl
          | BackgroudColor color -> { env with BackgroudColor = color } |> Async.Singleton
          | PenColor color -> { env with PenColor = color }  |> Async.Singleton
          | PenSize (Num size) -> { env with PenSize = size }  |> Async.Singleton
          | Sleep (Num sleep) -> 
            let mainCtx = myCanvas.getContext_2d()

            mainCtx.clearRect(0., 0., myCanvas.width, myCanvas.height)

            mainCtx.drawImage(U3.Case2 myCanvasBuffer, 0., 0., w * ratio, h  * ratio,  0., 0., w, h)
            if env.IsVisible then drawTurtle env
            async {
              do! Async.Sleep (int sleep)
              return env }
          | Comment s -> 
            printfn "comment: %s" s
            Async.Singleton env
          | ColorFill -> 
            drawContext ctx env (fun ctx ->
              let id = ctx.getImageData(0., 0.,  myCanvas.width,  myCanvas.height)
              let (r,g,b) = env.BackgroudColor.Rgb
              fill (int env.X) (int env.Y) (int id.width) (int id.height) id.data r g b
              ctx.putImageData (id, 0., 0.))
            env  |> Async.Singleton
          | ProcedureCall (name, args) -> 
            
            match Map.tryFind name env.Procedures with
            | Some x when x.Args.Length = args.Length -> 
              let variables = List.zip x.Args args |> List.fold (fun acc (key, expr) -> Map.add key (numExprEval env.Variables expr) acc) env.Variables
              exec { env with Variables = variables} x.Code
            | Some x -> failwithf "Wrong number of args for %s. Was %A, exected %A" name args x.Args
            | None -> failwithf "Unknown function %s" name
          | Circle (Num radius) ->
            let radius = float radius * ratio
            drawContext ctx env (fun ctx ->
              ctx.arc (float env.X, float env.Y, float radius, 0., 2. * Math.PI)
              ctx.moveTo(float env.X, float env.Y))
            env |> Async.Singleton
          | Disk (Num radius) ->
            let radius = float radius * ratio
            drawContext ctx env (fun ctx ->
              ctx.arc (float env.X, float env.Y, float radius, 0., 2. * Math.PI)
              ctx.fillStyle <- U3.Case1 env.BackgroudColor.Name
              ctx.strokeStyle <- U3.Case1 env.BackgroudColor.Name
              ctx.fill()
              ctx.moveTo(float env.X, float env.Y))
            env |> Async.Singleton
          | PenUp -> { env with Pen = Up } |> Async.Singleton
          | PenDown -> { env with Pen = Down } |> Async.Singleton
          | PenErase -> { env with Pen = Erase } |> Async.Singleton
          | HideTurtle -> { env with IsVisible = false } |> Async.Singleton
          | ShowTurtle -> { env with IsVisible = true } |> Async.Singleton
          | CleanScreen -> 
            let x = myCanvas.width / 2.
            let y = myCanvas.height / 2.
            ctx.beginPath()
            ctx.moveTo(x, y)
            ctx.clearRect(0., 0., myCanvas.width, myCanvas.height)
            { empty with X = x; Y = y; Procedures = env.Procedures; Variables = env.Variables } |> Async.Singleton
          | Procedure p -> { env with Procedures = Map.add p.Name p env.Procedures } |> Async.Singleton
          | Left (Num angle) -> { env with Angle = (env.Angle + int16 angle) % 360s } |> Async.Singleton
          | Right (Num angle) -> { env with Angle = (env.Angle - int16 angle) % 360s } |> Async.Singleton
          | Back (Num steps) -> 
            let steps = float steps * ratio
            let angleRadian = radConv * float env.Angle
            let moveY  = steps * (cos angleRadian)
            let moveX  = steps * (sin angleRadian)
            let x = env.X - moveX
            let y = env.Y - moveY

            if env.Pen = Up then ctx.moveTo(x, y)
            else 
              drawContext ctx env (fun ctx ->
                ctx.moveTo(float env.X, float env.Y)
                ctx.lineTo(x, y))

            { env with X = x; Y = y } |> Async.Singleton

          | Forward (Num steps) -> 
            let steps = float steps * ratio
            let angleRadian = radConv * float env.Angle
            let moveY  = steps * (cos angleRadian)
            let moveX  = steps * (sin angleRadian)
            let x = env.X + moveX
            let y = env.Y + moveY
            
            if env.Pen = Up then ctx.moveTo(x, y)
            else 
              drawContext ctx env (fun ctx ->
                ctx.moveTo(float env.X, float env.Y)
                ctx.lineTo(x, y))

            { env with X = x; Y = y } |> Async.Singleton
          | Loop (Num n, expr) -> loop n env expr
        return! exec env xs }
  and loop n (env:Env) expr = async {
    if n <= 0us then return env
    else 
      let! env = exec env expr
      return! loop (n - 1us) env expr }
      
  async {
    try
      try
        hideError()
        myButton.disabled <- true
        
        let! env = exec empty (CleanScreen :: expr)

        let mainCtx = myCanvas.getContext_2d()

        mainCtx.clearRect(0., 0., myCanvas.width, myCanvas.height)
        
        mainCtx.drawImage(U3.Case2 myCanvasBuffer, 0., 0., w * ratio, h  * ratio,  0., 0., w, h)
        if env.IsVisible then drawTurtle env
        myButton.classList.add "btn-success"
      finally
        myButton.classList.remove "btn-secondary"
        myButton.disabled <- false
    with e ->
      showError ()
      
      eprintfn "An exception %O occured when processing %A" e expr } |> Async.Start
  


myButton.addEventListener("click", fun _ -> 
  let sourceCode = window?editor?getValue()
  window.location.hash <- LZMA.compressToBase64 sourceCode
  let r = P.parse sourceCode
  printfn "result = %A" r
  match r with
  | Error _ -> showError ()
  | Ok (r, _) -> exec myCanvas r |> ignore)

let windHash = window.location.hash
if isNull windHash |> not && windHash.Length > 1 then
  let code = windHash.[1..]
  let code = LZMA.decompressFromBase64 code
  printf "%A" code
  window?editor?setValue code

  match P.parse code with
  | Error _ -> showError ()
  | Ok (r, _) -> exec myCanvas r |> ignore
