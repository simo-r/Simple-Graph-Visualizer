open System.Numerics

open System
open System.Drawing
open System.Drawing
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms

type WVMatrix() =
  let wv = new Drawing2D.Matrix()
  let vw = new Drawing2D.Matrix()

  member this.TranslateW(tx, ty) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleW(sx, sy) =
    wv.Scale(sx, sy)
    vw.Scale(1.f / sx, 1.f / sy, Drawing2D.MatrixOrder.Append)

  member this.RotateW(a) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateV(a) =
    vw.Rotate(a)
    wv.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.TranslateV(tx, ty) =
    vw.Translate(tx, ty)
    wv.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.ScaleV(sx, sy) =
    vw.Scale(sx, sy)
    wv.Scale(1.f / sx, 1.f / sy, Drawing2D.MatrixOrder.Append)

  member this.TransformPointV(p : PointF) =
    let a = [| p |]
    vw.TransformPoints(a)
    a.[0]

  member this.TransformPointW(p : PointF) =
    let a = [| p |]
    wv.TransformPoints(a)
    a.[0]

  member this.VW = vw
  member this.WV = wv

type LWCControl() =
  let wv = WVMatrix()
  let mutable size = SizeF()
  let mutable pos = PointF()
  let mutable parent : LWCContainer option = None
  member this.WV = wv
  abstract OnPaint : PaintEventArgs -> unit
  override this.OnPaint(e) = ()
  abstract OnMouseClick : MouseEventArgs -> unit
  override this.OnMouseClick(e) = ()
  abstract OnMouseDown : MouseEventArgs -> unit
  override this.OnMouseDown(e) = ()
  abstract OnMouseUp : MouseEventArgs -> unit
  override this.OnMouseUp(e) = ()
  abstract OnMouseMove : MouseEventArgs -> unit
  override this.OnMouseMove(e) = ()
  abstract HitTest : Point -> bool

  member this.Invalidate() =
    match parent with
    | Some p -> p.Invalidate()
    | None -> ()

  // Hit test usando un rettangolo di contenimento
  override this.HitTest(p : Point) =
    let pt = wv.TransformPointV(PointF(single p.X, single p.Y))
    let boundingbox = RectangleF(0.f, 0.f, size.Width, size.Height)
    boundingbox.Contains(pt)


  member this.ClientSize
    with get () = size
    and set (v) = 
      size <- v 
      this.Invalidate()

  member this.Position
    with get () = pos
    and set (v) =
      wv.TranslateV(pos.X, pos.Y)
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y) 
      this.Invalidate()

  member this.PositionInt = Point(int pos.X, int pos.Y)
  member this.ClientSizeInt = Size(int size.Width, int size.Height)
  member this.Left = pos.X
  member this.Top = pos.Y
  member this.Width
    with get () = size.Width
    and set (w) = size.Width <- w
  member this.Height
    with get () = size.Height
    and set (h) = size.Height <- h

  member this.Parent
    with get () = parent
    and set (v) = parent <- v

   (*NODE TYPE*)
and LWCNode() =
  inherit LWCControl()

  let mutable aangle = 0
  let mutable startOffset = None
  member this.Radius = float (this.Width / 2.f)
  member this.Angle
    with get () = aangle
    and set (v) = aangle <- v

  override this.OnPaint(e) =
    let g = e.Graphics
    g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Width, this.Height)
    g.DrawEllipse(Pens.Black, 0.f, 0.f, this.Width, this.Height)
    
  
  (*member this.RotatePoint(x,y) =
    PointF(x * float32 (Math.Cos(float this.Angle)) - y * float32 (Math.Sin(float this.Angle)),
                                x* float32 (Math.Sin(float this.Angle)) + y * float32 (Math.Cos(float this.Angle)))*)
  override this.OnMouseDown(e) =
    match startOffset with
      | None ->
        startOffset <- Some((PointF(single e.Location.X, single e.Location.Y)))
      | Some c -> ()
    printfn "NODE %A" e.Location

  override this.OnMouseMove(e) =
    printfn "MOUSE MOVE %A" e.Location
    match startOffset with
      | Some c ->
        let currPoint = (*c.WV.TransformPointW*)(PointF(single e.Location.X + this.Left, single e.Location.Y + this.Top))
        printfn "CURR POINT %A  %A %A %A" currPoint.X currPoint.Y this.Left this.Top
        let newP = PointF(currPoint.X - c.X, currPoint.Y - c.Y)
        this.Position <- PointF (newP.X , newP.Y )
      | None -> ()
    this.Invalidate()

  override this.OnMouseUp(e) =
    startOffset <- None

  override this.HitTest(p) =
    let pt = this.WV.TransformPointV(PointF(single p.X, single p.Y))
    let distance = Math.Sqrt(Math.Pow(float pt.X - this.Radius, 2.0) + Math.Pow(float pt.Y - this.Radius, 2.0))
    distance < float this.Radius

(*UI BUTTON*)
and LWCButton() =
  inherit LWCControl()
  let mutable name = "Button"
  let mFont = new Font("Arial", 18.f)
  abstract Operation : unit -> unit
  override this.Operation() = ()

  override this.OnPaint(e) =
    let g = e.Graphics
    g.DrawString(this.Name, this.Font, Brushes.Red, 0.f, 0.f)
    g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Width - 2.f, this.Height - 2.f)

  override this.OnMouseDown(e) =
    printfn "NODE %A" e.Location

  override this.OnMouseUp(e) =
    printfn "MOUSE CLICK %A COORDS %A" this.Name e.Location
    this.Operation()

  member this.Name
    with get () = name
    and set (n) = name <- n

  member this.Font = mFont

and LWCircleButton() =
  inherit LWCButton()


  override this.Operation() =
    let node = new LWCNode(Position = PointF(200.f, 200.f), ClientSize = SizeF(50.f, 50.f))
    match this.Parent with
      | Some p ->
        p.AddControl(node)
        p.Invalidate()
      | None -> ()


and LWArcButton() =
  inherit LWCButton()

  override this.Operation() = ()



and LWDownButton() =
  inherit LWCButton()
  override this.Operation() =
    printfn "DOWN BUTTON OPERATION"
    match this.Parent with
    | Some p -> (p.TranslateControls(0.f, 2.f))
    | None -> ()
    this.Invalidate()

and LWUpButton() =
  inherit LWCButton()
  override this.Operation() =
    printfn "DOWN BUTTON OPERATION"
    match this.Parent with
    | Some p -> (p.TranslateControls(0.f, -2.f))
    | None -> ()
    this.Invalidate()

and LWLeftButton() =
  inherit LWCButton()
  override this.Operation() =
    printfn "DOWN BUTTON OPERATION"
    match this.Parent with
    | Some p -> (p.TranslateControls(-2.f, 0.f))
    | None -> ()
    this.Invalidate()

and LWRightButton() =
  inherit LWCButton()
  override this.Operation() =
    printfn "DOWN BUTTON OPERATION"
    match this.Parent with
    | Some p -> (p.TranslateControls(2.f, 0.f))
    | None -> ()
    this.Invalidate()

and LWRotateLButton() =
  inherit LWCButton()

  override this.Operation() =
    printfn "ROTATE LEFT OPERATION"
    match this.Parent with
      | Some p -> p.RotateControls(5)
      | None -> ()
    this.Invalidate()
and LWRotateRButton() =
  inherit LWCButton()

  override this.Operation() =
    printfn "ROTATE LEFT OPERATION"
    match this.Parent with
      | Some p -> p.RotateControls(-5)
      | None -> ()
    this.Invalidate()


(*CONTAINER PER I CONTROLLI GRAFICI*)
and LWCContainer() as this =
  inherit UserControl()
  // Matrice per traslare il mondo del grafo
  let lwUIControls = ResizeArray<LWCButton>()
  let lwControls = ResizeArray<LWCNode>()
  let mutable selected : LWCControl option = None

  do
    this.DoubleBuffered <- true

  member this.TranslateControls(dx, dy) =
    lwControls |> Seq.iter (fun c ->
      c.Position <- new PointF(c.Left + dx, c.Top + dy)
    )

  member this.RotateControls(alpha) =
    lwControls |> Seq.iter (fun c ->
       let posX = 250.f - c.Width / 2.f (*c.Left + float32 c.Radius*)
       let posY = 250.f - c.Width / 2.f (* c.Top + float32 c.Radius*)
       //c.Angle <- c.Angle + alpha
       c.WV.TranslateV(posX, posY)
       c.WV.RotateV(float32 alpha)
       c.WV.TranslateV(-(posX), -(posY))
       ) //c.Position <- c.WV.TransformPointW(c.Position)



  member this.AddUIControls(c : LWCButton) =
    lwUIControls.Add(c)
    c.Parent <- Some(this)

  member this.AddControl(c) =
    lwControls.Add(c)
    c.Parent <- Some(this)

  override this.OnPaint(e) =
    let g = e.Graphics
    g.SmoothingMode <- SmoothingMode.HighQuality
    //TESTING
    g.DrawRectangle(Pens.Black, 0, 0, this.Width - 5, this.Height - 5)
    lwControls
    |> Seq.iter (fun c ->
      let bkg = e.Graphics.Save()
      // esercizio: impostare il rettangolo in client space
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))
      //bug: non supporta la rotazione
      e.Graphics.Transform <- c.WV.WV
      e.Graphics.SetClip (new RectangleF(PointF(-1.f, -1.f), SizeF(c.ClientSize.Width + 2.f, c.ClientSize.Height + 2.f)))
      g.DrawRectangle (Pens.Red, Rectangle(Point(-1, -1), Size(int c.ClientSize.Width + 2, int c.ClientSize.Height + 2)))
      c.OnPaint(evt)
      e.Graphics.Restore(bkg))
    (*UI CONTROLS*)
    lwUIControls
    |> Seq.iter (fun c ->
         let bkg = e.Graphics.Save()
         // esercizio: impostare il rettangolo in client space
         c.ClientSize <- g.MeasureString(c.Name, c.Font)
         let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))

         //bug: non supporta la rotazione
         e.Graphics.SetClip(new RectangleF(c.Position, c.ClientSize))
         e.Graphics.Transform <- c.WV.WV
         c.OnPaint(evt)
         e.Graphics.Restore(bkg))



  override this.OnMouseDown(e) =
    let oc = lwUIControls |> Seq.tryFindBack (fun c -> c.HitTest(e.Location))
    match oc with
    | Some c ->
      let p = c.WV.TransformPointV(PointF(single e.X, single e.Y))
      let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
      c.OnMouseDown(evt)
      selected <- Some(c :> LWCControl)
      printfn "%A" selected.Value
    | None ->
      (*MATCH CONTROLS*)
      let o = lwControls |> Seq.tryFindBack (fun c -> c.HitTest(e.Location))
      match o with
        | Some c ->
          let p = (*c.WV.TransformPointV*)(PointF(single e.X - c.Left, single e.Y - c.Top))
          let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
          c.OnMouseDown(evt)
          selected <- Some(c :> LWCControl)
          printfn "%A" selected.GetType
        | None -> ()

  override this.OnMouseMove(e) =
    match selected with
      | Some c ->
        printfn "MAI MOUSE MOVE %A" e.Location
        let p = (*c.WV.TransformPointV*)PointF(single e.X - c.Left, single e.Y - c.Top)
        let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
        c.OnMouseMove(evt)
      | None -> ()


  override this.OnMouseUp(e) =
    match selected with
      | Some c ->
        // TODO MODIFICARE STO NOME
        c.OnMouseUp(e)
        selected <- None
      | None -> ()


let f = new Form(TopMost = true, Size = Size(500, 500))
let container = new LWCContainer(Dock = DockStyle.Fill)
let mutable offsetY = 0.f
let circleButton = new LWCircleButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(200.f, 30.f), Name = "Circle")
offsetY <- offsetY + single circleButton.Font.Height
let upButton =
  new LWUpButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "U")
offsetY <- offsetY + single upButton.Font.Height
let downButton =
  new LWDownButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "D")
offsetY <- offsetY + single downButton.Font.Height
let leftButton =
  new LWLeftButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "L")
offsetY <- offsetY + single leftButton.Font.Height
let rightButton =
  new LWRightButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "R")
offsetY <- offsetY + single rightButton.Font.Height
let rotateLButton =
  new LWRotateLButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "RL")
offsetY <- offsetY + single rotateLButton.Font.Height
let rotateRButton =
  new LWRotateRButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "RR")

container.AddUIControls(circleButton)
container.AddUIControls(upButton)
container.AddUIControls(downButton)
container.AddUIControls(leftButton)
container.AddUIControls(rightButton)
container.AddUIControls(rotateLButton)
container.AddUIControls(rotateRButton)
f.Controls.Add(container)
f.Resize.Add(fun e -> container.Invalidate())
f.Show()