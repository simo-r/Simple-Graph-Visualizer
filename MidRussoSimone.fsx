open System
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
    and set (v) = size <- v //this.Invalidate()
  
  member this.Position
    with get () = pos
    and set (v) =
      wv.TranslateV(pos.X, pos.Y)
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y) //this.Invalidate()
  
  member this.PositionInt = Point(int pos.X, int pos.Y)
  member this.ClientSizeInt = Size(int size.Width, int size.Height)
  member this.Left = pos.X
  member this.Top = pos.Y
  member this.Width = size.Width
  member this.Height = size.Height
  
  member this.Parent
    with get () = parent
    and set (v) = parent <- v
   
   (*NODE TYPE*)
and LWCNode() =
  inherit LWCControl()
  
  override this.OnPaint(e) =
    let g = e.Graphics
    g.DrawRectangle(Pens.Black,0.f,0.f,this.Width,this.Height)
    g.DrawEllipse(Pens.Black,0.f,0.f,this.Width-3.f,this.Height-3.f)

(*UI BUTTON*)
and LWCButton() =
  inherit LWCControl()
  let mutable name = "Button"
  let mFont = new Font("Arial", 18.f)
  abstract Operation : unit -> unit
  override this.Operation() = ()
  
  override this.OnPaint(e) =
    let g = e.Graphics
    printfn "%A %A" this.Width this.Height
    g.DrawString(this.Name, this.Font, Brushes.Red, 0.f, 0.f)
    g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Width - 2.f, this.Height - 2.f)
  
  override this.OnMouseClick(e) =
    printfn "MOUSE CLICK %A COORDS %A" this.Name e.Location
    this.Operation()
  
  member this.Name
    with get () = name
    and set (n) = name <- n
  
  member this.Font = mFont

and LWCircleButton() =
  inherit LWCButton()
  override this.Operation() = (
    let x = this.Width
    let node = new LWCNode(Position = PointF(200.f,200.f), ClientSize = SizeF(30.f,30.f))
    match this.Parent with
      | Some p -> 
        p.AddControl(node)
        p.Invalidate()
      | None -> ()
  
  )

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

(*CONTAINER PER I CONTROLLI GRAFICI*)
and LWCContainer() =
  inherit UserControl()
  // Matrice per traslare il mondo del grafo
  let lwUIControls = ResizeArray<LWCButton>()
  let lwControls = ResizeArray<LWCNode>()
  let mutable selected : LWCControl option = None
  
  member this.TranslateControls(dx, dy) = 
    lwControls |> Seq.iter( fun c -> 
      c.Position <- new PointF(c.Left + dx, c.Top + dy)
    )
   
  
  member this.AddUIControls(c : LWCButton) =
    lwUIControls.Add(c)
    c.Parent <- Some(this)
    
  member this.AddControl( c) =
    lwControls.Add(c)
    c.Parent <- Some(this)
  
  override this.OnPaint(e) =
    let g = e.Graphics
    g.SmoothingMode <- SmoothingMode.HighQuality
    //TESTING
    g.DrawRectangle(Pens.Black, 0, 0, this.Width - 5, this.Height - 5)
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
    lwControls 
    |> Seq.iter(fun c ->
      let bkg = e.Graphics.Save()
      // esercizio: impostare il rettangolo in client space
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
      selected <- Some (c :> LWCControl)
      printfn "%A" selected.Value
    | None -> 
      let o = lwControls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
      match o with 
        | Some c ->
          selected <- Some ( c:> LWCControl)
          printfn "%A" selected.GetType
        | None -> ()
        
  override this.OnMouseUp(e) =
    match selected with
      | Some c ->
        // TODO MODIFICARE STO NOME
        c.OnMouseClick(e)
        selected <- None
      | None -> ()
  

let f = new Form(TopMost = true)
let container = new LWCContainer(Dock = DockStyle.Fill)
let circleButton = new LWCircleButton(Position = PointF(0.f, 0.f), ClientSize = SizeF(200.f, 30.f), Name = "Circle")
let upButton =
  new LWUpButton(Position = PointF(0.f, single circleButton.Font.Height), ClientSize = SizeF(20.f, 20.f), Name = "U")
let downButton =
  new LWDownButton(Position = PointF(0.f, single (upButton.Font.Height + circleButton.Font.Height)), ClientSize = SizeF(20.f, 20.f), Name = "D")

container.AddUIControls(circleButton)
container.AddUIControls(upButton)
container.AddUIControls(downButton)
f.Controls.Add(container)
f.Resize.Add(fun e -> container.Invalidate())
f.Show()