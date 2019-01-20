open System.Windows.Forms

open System.Numerics

open System
open System.Drawing
open System.Drawing
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms

type ArcAutomaton = Click1 | Click2 | Nothing


type WVMatrix() =
  let mutable wv = new Drawing2D.Matrix()
  let mutable vw = new Drawing2D.Matrix()

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

  member this.VW 
   with get() = vw
   and set(v) = vw <- v
  member this.WV 
    with get() = wv
    and set(v) = wv <- v
    

type NodePair = LWCNode * LWCNode

and NodeDepth = LWCNode * int

and  LWCControl() =
  let mutable wv = WVMatrix()
  let mutable size = SizeF()
  let mutable pos = PointF()
  let mutable parent : LWCContainer option = None
  let mutable name : String = "T"
  let mutable nameRect : RectangleF = RectangleF()
  let mutable mFont = new Font("Arial", 18.f)
  let mutable moving = false
  
  member this.Moving 
    with get() = moving
    and set(v) = moving <- v
    
  member this.WV 
    with get() = wv
    and set(v) = wv <- v
    
  member this.Name 
    with get() = name
    and set(v) = 
      
      name <- v
      this.Invalidate()
  
  member this.Font 
    with get() = mFont
    and set(v) = mFont <- v
  
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
      nameRect <- RectangleF(0.f,size.Height / 2.f,size.Width, 10.f)
      //this.Invalidate()

  member this.Position
    with get () = pos
    and set (v) =
      wv.TranslateV(pos.X, pos.Y)
      pos <- v
      wv.TranslateV(-pos.X, -pos.Y) 
      //this.Invalidate()

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
    and set (v) = 
      parent <- v
      
  member this.NameRect 
    with get() = nameRect
    
      

   (*NODE TYPE*)
and LWCNode() =
  inherit LWCControl()
  
  let mutable aangle = 0
  let mutable startOffset = None
  let arcs = ResizeArray<LWCArc>()
  
  let mutable draggingOffset = PointF()
  
  member this.Radius = float (this.Width / 2.f)
  member this.Angle
    with get () = aangle
    and set (v) = aangle <- v
    
  member this.DraggingOffset = draggingOffset
    
  member this.Arcs = arcs

  member this.AddArc(a) = 
    arcs.Add(a)
    
  override this.OnPaint(e) =
    let g = e.Graphics
    //g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Width, this.Height)
    g.DrawEllipse(Pens.Black, 0.f, 0.f, this.Width, this.Height)
    let nameRect = Rectangle( int this.NameRect.X,int this.NameRect.Y, int this.NameRect.Width, int this.NameRect.Height)
    //g.DrawRectangle(Pens.Black,nameRect)
    let m = g.MeasureString(this.Name, this.Font)
    g.DrawString(this.Name,this.Font,Brushes.Black,(this.Width - m.Width)/2.f,(this.Height - m.Height) / 2.f)
    
  
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
    this.Moving <- true
    printfn "MOUSE MOVE %A" e.Location
    match startOffset with
      | Some c ->
        // newP = + left + (moveOffSet  - starOffSet)
        let offSet = PointF(float32 e.Location.X - c.X, float32 e.Location.Y - c.Y)
        let currPoint = (*c.WV.TransformPointW*)(PointF(single e.Location.X + this.Left, single e.Location.Y + this.Top))
        let newP = PointF(currPoint.X - c.X, currPoint.Y - c.Y)
        this.Position <- PointF (newP.X , newP.Y )
        if(arcs.Count > 0) then
          this.MoveArcs()
          this.IncreaseDragOffset(offSet)
          this.Parent.Value.Drag(this,offSet)
      | None -> ()
    this.Invalidate()
    
  member this.IncreaseDragOffset(offSet) =
    draggingOffset <- PointF(draggingOffset.X + offSet.X, draggingOffset.Y + offSet.Y)
    
  member this.ElasticForward(offSet:PointF) =
    this.IncreaseDragOffset(offSet)
    this.Position <- PointF (this.Left + offSet.X, this.Top + offSet.Y)
    this.MoveArcs()
    
  
    
  member this.ElasticBack(offSet:PointF,percentage) =
    this.Position <- PointF (this.Left + offSet.X * percentage, this.Top + offSet.Y * percentage)
    this.MoveArcs()
  
  member this.StopElasticBack() =
    draggingOffset <- PointF()

  override this.OnMouseUp(e) =
    if(not this.Moving && this.NameRect.Contains(PointF(float32 e.Location.X, float32 e.Location.Y))) then 
        printfn " DENTRO"
        this.Parent.Value.ListenForName(this)
    match startOffset with
      | Some _ -> 
        startOffset <- None
        this.Moving <- false
        this.Parent.Value.EnableElasticMovement()
      | None -> ()
    printfn " ON MOUSE UP %A" e.Location
    
   

  override this.HitTest(p) =
    let pt = this.WV.TransformPointV(PointF(single p.X, single p.Y))
    let distance = Math.Sqrt(Math.Pow(float pt.X - this.Radius, 2.0) + Math.Pow(float pt.Y - this.Radius, 2.0))
    distance < float this.Radius
    
  member this.MoveArcs() =
    arcs |> Seq.iter( fun arc ->
      arc.Move()    
    )

and LWCArc(nodes) as this =
  inherit LWCControl()
  
  let mutable startOffset = None
  let mutable endP = PointF()
  let pen = new Pen(Color.LightBlue, this.Height)
  let nodes : NodePair = nodes
      
  
  member this.Nodes = nodes
  
  member this.GetHalfWidth(p1:PointF,p2:PointF) =
    float32 (Math.Sqrt(Math.Pow(float(p1.X -  p2.X),2.0)
     + Math.Pow(float(p1.Y -  p2.Y),2.0) ) / 2.0)
  
  member this.PenWidth
    with get() = pen.Width
    and set(v) =
      //this.Height <- v
      pen.Width <- v
      this.Font <- new Font("Arial",pen.Width)
  
  override this.OnPaint(e) =
    let g = e.Graphics
    //g.FillRectangle(Brushes.Black, 0.f, 0.f, this.Width, this.Height)
    g.DrawLine(pen, 0.f, pen.Width, this.Width, pen.Width)
    let m = g.MeasureString(this.Name, this.Font)
    g.DrawString(this.Name,this.Font,Brushes.Black,(this.Width - m.Width)/2.f,(this.Height - m.Height) / 2.f)
    
  
  override this.OnMouseDown(e) = ()
    (*match startOffset with
      | None ->
        startOffset <- Some((PointF(single e.Location.X, single e.Location.Y)))
      | Some c -> ()
    printfn "NODE %A" e.Location*)
  
  override this.OnMouseMove(e) = ()
    (*printfn "MOUSE MOVE %A" e.Location
    match startOffset with
      | Some c ->
        let currPoint = (*c.WV.TransformPointW*)(PointF(single e.Location.X + this.Left, single e.Location.Y + this.Top))
        printfn "CURR POINT %A  %A %A %A" currPoint.X currPoint.Y this.Left this.Top
        let newP = PointF(currPoint.X - c.X, currPoint.Y - c.Y)
        this.Position <- PointF (newP.X , newP.Y )
      | None -> ()
    this.Invalidate()*)
  
  override this.OnMouseUp(e) = 
    this.Parent.Value.ListenForName(this)
    
  member this.Move() =
    let startNode,endNode = 
      match nodes with
        | l1,l2 ->l1, l2
    let p1 = startNode.WV.TransformPointW(PointF(float32 startNode.Radius,float32 startNode.Radius))
    let p2 = endNode.WV.TransformPointW(PointF(float32 endNode.Radius,float32 endNode.Radius))
    let h = 5.f / this.Parent.Value.ScaleFactor
    let h2 = 10.f / this.Parent.Value.ScaleFactor
    let angle = Math.Atan2((float(p2.Y - p1.Y)) , float(p2.X - p1.X)) * 180.0 / Math.PI
    let ipo = float32 (Math.Sqrt(Math.Pow(float(p2.X - p1.X),2.0) + Math.Pow(float(p2.Y - p1.Y),2.0)))
    let arc = new LWCArc(NodePair(startNode,endNode),Position = p1, ClientSize = SizeF(ipo, h2),EndP = p2, PenWidth = h)
    arc.WV.TranslateV(p1.X, p1.Y)
    arc.WV.RotateV(float32 -angle)
    arc.WV.TranslateV(-(p1.X), -(p1.Y))
    this.WV <- arc.WV
    this.Position <- arc.Position
    this.ClientSize <- arc.ClientSize
    this.EndP <- arc.EndP
    this.PenWidth <- arc.PenWidth
     
  member this.EndP 
    with get() = endP
    and set(p) = 
      endP <- p
      //this.Invalidate()
    
  
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

  override this.Operation() =
    match this.Parent with
      | Some p ->
        p.ArcDrawing <- ArcAutomaton.Click1
      | None -> ()


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
      | Some p -> p.RotateControls(5.f)
      | None -> ()
    this.Invalidate()
and LWRotateRButton() =
  inherit LWCButton()

  override this.Operation() =
    printfn "ROTATE RIGHT OPERATION"
    match this.Parent with
      | Some p -> p.RotateControls(-5.f)
      | None -> ()
    this.Invalidate()
    
and LWZoomInButton() =
  inherit LWCButton()

  override this.Operation() =
    printfn "ZOOM IN OPERATION"
    match this.Parent with
      | Some p -> p.ZoomControls(1.f/2.f)
      | None -> ()
    this.Invalidate()
    
and LWZoomOutButton() =
  inherit LWCButton()

  override this.Operation() =
    printfn "ZOOM OUT OPERATION"
    match this.Parent with
      | Some p -> p.ZoomControls(2.f)
      | None -> ()
    this.Invalidate()
    
and LWKillButton() =
  inherit LWCButton()
  
  override this.Operation() =
      printfn "ZOOM IN OPERATION"
      match this.Parent with
        | Some p -> p.Killing <- true 
        | None -> ()
      this.Invalidate()
  
  


(*CONTAINER PER I CONTROLLI GRAFICI*)
and LWCContainer() as this =
  inherit UserControl()
  // Matrice per traslare il mondo del grafo
  let lwUIControls = ResizeArray<LWCButton>()
  let lwControls = ResizeArray<LWCControl>()
  let mutable selected : LWCControl option = None
  let mutable arcDrawing = ArcAutomaton.Nothing
  let wv = WVMatrix()
  
  let mutable p1 : PointF option = None
  let mutable p2 : PointF option = None
  let mutable firstNode : LWCNode option = None
  let mutable killing = false
  let mutable listeningForName = false 
  let mutable listeningComponent : LWCControl option = None
  let mutable listeningString = ""
  
  let mutable stop = false
  let mutable nodeNum = 0
  let timer = new Timer(Interval = 50,Enabled = false)
  let mutable times = 0;
  
  let mutable elasticBack = false
  
  do
    timer.Tick.Add( fun a ->
      lwControls |> Seq.iter( fun control ->
        match control with
          | (:? LWCNode as node) -> 
            let dragOffSet = PointF(-node.DraggingOffset.X,-node.DraggingOffset.Y)
            node.ElasticBack(dragOffSet,5.f/100.f)
            node.Invalidate()
            
          | _ -> ()
    
      )
      times <- times + 1
      if(times = 20) then
        times <- 0
        lwControls |> Seq.iter( fun control ->
          match control with
            | (:? LWCNode as node) -> 
              node.StopElasticBack()
              
            | _ -> ()
        )
        elasticBack <- false
        timer.Stop()
    )
    this.DoubleBuffered <- true
    
    
  member this.ScaleFactor
    with get() = wv.VW.Elements.[0]

  member this.TranslateControls(dx, dy) =
    if( not elasticBack) then
      lwControls |> Seq.iter (fun c ->
        c.Position <- new PointF(c.Left + dx, c.Top + dy)
      )
    
  member this.ListenForName(c: LWCControl) =
    listeningForName <- true
    listeningComponent <- Some (c)
    
  member this.ArcDrawing 
    with get() = arcDrawing
    and set(v) =
      if(v = ArcAutomaton.Click1) then 
        this.Cursor <- Cursors.Hand
      else if( v = ArcAutomaton.Nothing) then
        this.Cursor <- Cursors.Arrow
      arcDrawing <- v
      
  member this.Killing
    with get() = killing
    and set(v) =
      if(v) then 
        this.Cursor <- Cursors.Hand
      else 
        this.Cursor <- Cursors.Arrow
      killing <- v

  member this.RotateControls(alpha) =
    if( not elasticBack) then
      let posX = float32 (this.Width / 2)
      let posY = float32 (this.Height / 2)
      lwControls |> Seq.iter (fun c ->
         
         c.WV.TranslateV(posX, posY)
         c.WV.RotateV(float32 alpha)
         c.WV.TranslateV(-(posX), -(posY))
         ) 
       
  member this.ZoomControls(zoom) =
    if( not elasticBack) then
      wv.ScaleV(zoom,zoom)
      lwControls |> Seq.iter (fun c ->
        this.ZoomControl(c,zoom)
      ) 

  member this.ZoomControl(c,zoom) =
  
    let posX = float32 (this.Width / 2) 
    let posY = float32 (this.Height / 2) 
   
    c.WV.TranslateV(posX, posY)
    c.WV.ScaleV(zoom,zoom)
    c.WV.TranslateV(-(posX), -(posY))
    
  member this.AddUIControls(c : LWCButton) =
    lwUIControls.Add(c)
    c.Parent <- Some(this)

  member this.AddControl(c:LWCControl) =
    if( not elasticBack) then
      match c with
        | :? LWCArc as c-> ()
        | _ -> 
          nodeNum <- nodeNum + 1
          this.ZoomControl(c,wv.VW.Elements.[0])
      lwControls.Add(c)
      c.Parent <- Some(this)
    
  override this.OnPaint(e) =
    let g = e.Graphics
    g.SmoothingMode <- SmoothingMode.HighQuality
    //TESTING
    //g.DrawRectangle(Pens.Black, 0, 0, this.Width - 5, this.Height - 5)
    //g.DrawArc(Pens.Black,RectangleF(float32(this.Width / 2) - 50.f,float32(this.Height / 2)-50.f,50.f,50.f),0.f,360.f)
    lwControls
    |> Seq.iter (fun c ->
      let bkg = e.Graphics.Save()
      // esercizio: impostare il rettangolo in client space
      let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.ClientSizeInt))
      //bug: non supporta la rotazione
      e.Graphics.Transform <- c.WV.WV
      e.Graphics.SetClip (new RectangleF(PointF(-1.f, -1.f), SizeF(c.ClientSize.Width + 2.f, c.ClientSize.Height + 2.f)))
      //g.DrawRectangle (Pens.Red, Rectangle(Point(-1, -1), Size(int c.ClientSize.Width + 2, int c.ClientSize.Height + 2)))
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
    if(not killing && arcDrawing = ArcAutomaton.Nothing) then
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
            selected <- Some(c)
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
        // TODO MODIFICARE STO NOM
        printfn " QUAAAAAAAAAAAAAAA"
        let p = c.WV.TransformPointV(PointF(single e.X , single e.Y ))
        let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
        c.OnMouseUp(evt)
        selected <- None
      | None -> ()
      
      
  
  
  override this.OnMouseClick(e) =
    if(killing && not elasticBack) then
      let o = lwUIControls |> Seq.tryFindBack (fun c -> c.HitTest(e.Location))
      match o with
        | Some c-> ()
        | None ->
          let x = lwControls |> Seq.tryFindBack (fun c -> c.HitTest(e.Location))
          match x with
            | Some c ->
              lwControls.Remove(c) |> ignore
              match c with
                | (:? LWCNode as node) ->
                  //Rimuovi tutti gli archi
                  nodeNum <- nodeNum - 1
                | _ -> ()
              this.Invalidate() 
            | None -> ()
      this.Killing <- false 
      
    
    match arcDrawing with
      | ArcAutomaton.Click1 ->
        let o = lwControls |> Seq.tryFindBack (fun c -> c.HitTest(e.Location))
        match o with
          | Some(:? LWCNode as c) -> 
            printfn "CLICK1"
            // 0 1 2
            // 3 4 5
            c.WV.VW.Elements |> Seq.iter(fun c -> printfn "CLICK1 %A" c)
            let pos = c.WV.TransformPointW(PointF(float32 c.Radius,float32 c.Radius))
            p1 <- Some((PointF(pos.X (*+ float32 c.Radius*), pos.Y (*+ float32 c.Radius*))))
            firstNode <- Some(c)
            this.ArcDrawing <- ArcAutomaton.Click2
          | None | Some _ -> 
            firstNode <- None
            p1 <- None
            this.ArcDrawing <- ArcAutomaton.Nothing
      
      | ArcAutomaton.Click2 ->
        let o = lwControls |> Seq.tryFindBack (fun c -> c.HitTest(e.Location))
        match o with
          | Some(:? LWCNode as c) when c <> firstNode.Value -> 
            printfn "CLICK2"
            c.WV.VW.Elements |> Seq.iter(fun c -> printfn "CLICK1 %A" c)
            let pos = c.WV.TransformPointW(PointF(float32 c.Radius,float32 c.Radius))
            let h = 5.f / wv.VW.Elements.[0]
            let h2 = 10.f / wv.VW.Elements.[0]
            p2 <- Some((PointF(pos.X , pos.Y)))
            let angle = Math.Atan2((float(p2.Value.Y - p1.Value.Y)) , float(p2.Value.X - p1.Value.X)) * 180.0 / Math.PI
            printfn "ANGLE %A HEIGHT %A" angle h
            let ipo = float32 (Math.Sqrt(Math.Pow(float(p2.Value.X - p1.Value.X),2.0) + Math.Pow(float(p2.Value.Y - p1.Value.Y),2.0)))
            let arc = new LWCArc(NodePair(firstNode.Value,c),Position = p1.Value, ClientSize = SizeF(ipo, h2),EndP = p2.Value, PenWidth = h)
            firstNode.Value.AddArc(arc)
            
            c.AddArc(arc)
            arc.WV.TranslateV(p1.Value.X, p1.Value.Y)
            arc.WV.RotateV(float32 -angle)
            arc.WV.TranslateV(-(p1.Value.X), -(p1.Value.Y))
            this.AddControl(arc)
            this.Invalidate()
          | None | Some _ -> ()
          
            
        firstNode <- None
        p1 <- None
        p2 <- None
        this.ArcDrawing <- ArcAutomaton.Nothing
      | _ -> ()
      
    
  override this.OnKeyPress(e) =
    match listeningForName,listeningComponent with
      | true, Some(c) ->
        listeningForName <- false
        c.Name <- e.KeyChar.ToString()
      | _ -> ()
      
      
  
    
  member this.Drag(node,offSet) =
      //draggingOffset <- PointF(draggingOffset.X + offSet.X,draggingOffset.Y + offSet.Y)
      let mutable visited = 0
      let mutable depth = 1
      let nodeList : ResizeArray<NodeDepth> = ResizeArray<NodeDepth>()
      nodeList.Add(NodeDepth(node,depth))
      depth <- depth + 1
      let mutable currNode = node
      while visited < (nodeNum) && (visited < nodeList.Count) do //(
        printfn "ZZ ENTER %A %A" visited nodeList.Count
        match nodeList.[visited] with
          | node, _ -> currNode <- node 
        printfn "ZZ AFTER"
        currNode.Arcs |> Seq.iter(fun a ->
          match a.Nodes with
            | a1,a2 ->
              printfn "ZZ A1A2"
              let mutable nnode = nodeList |> Seq.tryFind( fun nod ->
                match nod with
                  | n,_ -> n = a1)
              match nnode with
                | None ->
                  printfn "ZZ ADD1"
                  nodeList.Add(NodeDepth(a1,depth))
                  //a1.ElasticForward(offSet)
                | Some(n) ->
                  nnode <- nodeList |> Seq.tryFind (fun nod -> 
                    match nod with
                      | n,_ -> n = a2
                  )
                  match nnode with
                    | None -> 
                      printfn "ZZ ADD2"
                      nodeList.Add(NodeDepth(a2,depth))
                    | Some(_) -> ()
          
        )
        depth <- depth + 1
        visited <- visited + 1
        printfn " VISITED %A NODE NUM %A" visited nodeNum
        
      //)
      nodeList |> Seq.iter( fun c ->
        match c with
          | n,d -> printfn "NODE  %A %A " n.Name d
      )
      nodeList.RemoveAt(0)
      this.DragMove(nodeList,offSet)
      
        //visited++
  member this.DragMove(nodeList,offSet) =
    nodeList |> Seq.iter( fun nn -> 
      match nn with
        | node,depth ->
          let scaledOffSet = PointF(offSet.X / float32 depth, offSet.Y /float32 depth)
          node.ElasticForward(scaledOffSet)
    )
    
  member this.EnableElasticMovement() =
    elasticBack <- true
    timer.Start()
  
  
  
    
    
      


let f = new Form(TopMost = true, Size = Size(500, 500))
let container = new LWCContainer(Dock = DockStyle.Fill)
let mutable offsetY = 0.f
let circleButton = new LWCircleButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(200.f, 30.f), Name = "Circle")
offsetY <- offsetY + single circleButton.Font.Height
let arcButton = new LWArcButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(200.f, 30.f), Name = "Arc")
offsetY <- offsetY + single arcButton.Font.Height
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
offsetY <- offsetY + single rotateRButton.Font.Height
let zoomInButton =
  new LWZoomInButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "ZI")
offsetY <- offsetY + single zoomInButton.Font.Height
let zoomOutButton =
  new LWZoomOutButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "ZO")
offsetY <- offsetY + single zoomOutButton.Font.Height
let killButton =
  new LWKillButton(Position = PointF(0.f, offsetY), ClientSize = SizeF(20.f, 20.f), Name = "KILL")

container.AddUIControls(circleButton)
container.AddUIControls(upButton)
container.AddUIControls(downButton)
container.AddUIControls(leftButton)
container.AddUIControls(rightButton)
container.AddUIControls(rotateLButton)
container.AddUIControls(rotateRButton)
container.AddUIControls(zoomInButton)
container.AddUIControls(zoomOutButton)
container.AddUIControls(arcButton)
container.AddUIControls(killButton)
f.Controls.Add(container)
f.Resize.Add(fun e -> container.Invalidate())
f.Show()

