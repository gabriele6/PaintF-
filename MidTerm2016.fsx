open System.Windows.Forms
open System.Drawing

#load "LWCs.fsx"

open LWCs

type BouncingBall() =
  let mutable location = PointF()
  let mutable speed = SizeF(10.f,10.f) // px/s
  let mutable size = SizeF(25.f,25.f)
  let mutable lastT = System.DateTime.Now

  member this.Location with get() = location and set(v) = location <- v
  member this.Speed with get() = speed and set(v) = speed <- v
  member this.Size with get() = size and set(v) = size <- v
  member this.Bounds = new RectangleF(location, size)
  member this.CenterLocation with get() = PointF(location.X + size.Width / 2.f, location.Y + size.Height / 2.f)

  member this.UpdateSpeed(balls:BouncingBall seq) =
    let vb (b:BouncingBall) (vx : single, vy : single) =
      let ct, cb = this.CenterLocation, b.CenterLocation
      let cx, cy = cb.X - ct.X, cb.Y - ct.Y
      let sp = cx*vx + cy*vy
      let cr, vr = sqrt(cx*cx + cy*cy), sqrt(vx*vx+vy*vy)
      let cosa = sp / (cr*vr)
      let sina = sqrt(1.f - cosa*cosa)
      let vtb, votb = - vr * cosa, vr * sina
      let beta = atan2 cy  cx
      let pi = single(System.Math.PI)
      let vtbx, vtby = vtb * cos(beta), vtb * sin(beta)
      let votbx, votby = votb * cos(pi / 2.f - beta), votb * sin(pi / 2.f - beta)
      (votbx + vtbx, votby + vtby)
    
    for b in balls do
      if b <> this && this.CollideWith(b) then
        let vx, vy = vb b (speed.Width, speed.Height)
        speed <- SizeF(vx, vy)

  member this.UpdatePosition() =
    let t = System.DateTime.Now
    let dt = t - lastT
    let vx = speed.Width / 1000.f
    let vy = speed.Height / 1000.f
    let dx = vx * single(dt.TotalMilliseconds)
    let dy = vy * single(dt.TotalMilliseconds)
    let rnd = new System.Random()
    location <- PointF(location.X + dx, location.Y + dy)
    //location <- PointF(location.X + single(rnd.NextDouble()) * dx, location.Y + single(rnd.NextDouble()) * dy)
    //location <- PointF(single(rnd.NextDouble()) * 50.f, single(rnd.NextDouble()) * 50.f)
    lastT <- t

  member this.CollideWith (b:BouncingBall) =
    let sqdist (p1:PointF) (p2:PointF) =
      let dx, dy = p1.X - p2.X, p1.Y - p2.Y
      dx*dx + dy * dy
    let c1, c2 = this.CenterLocation, b.CenterLocation
    let d1, d2 = this.Size.Width / 2.f, b.Size.Width / 2.f
    sqdist c1 c2 <= (d1 + d2)*(d1 + d2)

type RoundButton() =
  inherit LWC()

  member val Text : string = "" with get, set
  member val pressed : bool = false with get, set

  override this.OnPaint e =
    let g = e.Graphics
    if not(this.pressed) then
        g.DrawEllipse(Pens.Black, 0.f, 0.f, this.Size.Width, this.Size.Height)
        let r = g.MeasureString(this.Text, this.Parent.Font)
        let x, y = (this.Size.Width - r.Width) / 2.f, (this.Size.Height - r.Height) / 2.f
        g.DrawString(this.Text, this.Parent.Font, Brushes.Black, PointF(x, y))
    else 
        g.DrawEllipse(Pens.Red, 0.f, 0.f, this.Size.Width, this.Size.Height)
        let r = g.MeasureString(this.Text, this.Parent.Font)
        let x, y = (this.Size.Width - r.Width) / 2.f, (this.Size.Height - r.Height) / 2.f
        g.DrawString(this.Text, this.Parent.Font, Brushes.Red, PointF(x, y))

type VWCoordinates() =
  let mutable wv = new Drawing2D.Matrix()
  let mutable vw = new Drawing2D.Matrix()

  member this.WV with get() = wv and set(v) = wv <- v
  member this.VW with get() = vw and set(v) = vw <- v

  member this.Clone() =
    let ret = VWCoordinates()
    ret.WV <- wv.Clone()
    ret.VW <- vw.Clone()
    ret

  member this.Multiply(m:VWCoordinates, ?order:Drawing2D.MatrixOrder) =
    let wo = match order with Some(Drawing2D.MatrixOrder.Append) -> Drawing2D.MatrixOrder.Append | _ -> Drawing2D.MatrixOrder.Prepend
    let vo = match order with Some(Drawing2D.MatrixOrder.Prepend) -> Drawing2D.MatrixOrder.Prepend | _ -> Drawing2D.MatrixOrder.Append
    wv.Multiply(m.WV, wo)
    vw.Multiply(m.VW, vo)

  member this.Rotate(a:single) =
    wv.Rotate(a)
    vw.Rotate(-a, Drawing2D.MatrixOrder.Append)

  member this.RotateAt(a:single, p:PointF) =
    wv.RotateAt(a, p)
    vw.RotateAt(-a, p, Drawing2D.MatrixOrder.Append)

  member this.Translate(tx:single, ty:single) =
    wv.Translate(tx, ty)
    vw.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

  member this.Scale(sx:single, sy:single) =
    wv.Scale(sx, sy)
    vw.Scale(1.f / sx, 1.f / sy, Drawing2D.MatrixOrder.Append)

  member this.TransformPointVW (p:Point) =
    let toPointF (p:Point) = PointF(single p.X, single p.Y)
    let a = [| p |> toPointF |]
    this.VW.TransformPoints(a)
    a.[0]

  member this.ScaleAtV(sx:single, sy: single, cv:Point) =
      let cw = cv |> this.TransformPointVW
      this.Scale(sx, sy)
      let cwp = cv |> this.TransformPointVW
      this.Translate(cwp.X - cw.X, cwp.Y - cw.Y)

type Mid() as this =
  inherit LWContainer()
  let mutable isPan = false
  let mutable isBox = false
  let mutable isDel = false

  let mutable wtransform : VWCoordinates option = None
  let rectangles = ResizeArray<RectangleF>()
  let rectdata = ResizeArray<VWCoordinates*RectangleF array>()


  //SFERE
  let mutable ballsSize = 25.f
  let balls = new ResizeArray<BouncingBall>()
  let mutable buffer : Bitmap = null //new Bitmap(this.Width, this.Height)
  let updateBuffer () =
    if buffer = null || buffer.Width <> this.Width || buffer.Height <> this.Height then
      if buffer <> null then buffer.Dispose()
      buffer <- new Bitmap(this.Width, this.Height)

  let updateBalls() =
    balls |> Seq.iter (fun b ->
      b.UpdatePosition()      
      //controllo collisione con finestra
      if b.Location.X < 0.f then
        if b.Speed.Width<=0.f then b.Speed <- SizeF(- b.Speed.Width, b.Speed.Height)
      if b.Location.X + b.Size.Width > single(this.Width) then
        if b.Speed.Width>=0.f then b.Speed <- SizeF(- b.Speed.Width, b.Speed.Height)
      if b.Location.Y < 0.f then
        if b.Speed.Height<=0.f then b.Speed <- SizeF(b.Speed.Width, - b.Speed.Height)
      if b.Location.Y + b.Size.Height > single(this.Height) then
        if b.Speed.Height>=0.f then b.Speed <- SizeF(b.Speed.Width, - b.Speed.Height)
      //controllo collisione con scatole
      let mutable index = rectangles.Count - 1
      while index >= 0 do
            if rectangles.[index].Contains(b.Location) then

                if rectangles.[index].Contains(b.Location.X+5.f, b.Location.Y) then
                    b.Speed <- SizeF(- b.Speed.Width, b.Speed.Height)
                if rectangles.[index].Contains(b.Location.X-5.f, b.Location.Y) then
                    b.Speed <- SizeF(- b.Speed.Width, b.Speed.Height)
                if rectangles.[index].Contains(b.Location.X, b.Location.Y+5.f) then
                    b.Speed <- SizeF(b.Speed.Width, -b.Speed.Height)
                if rectangles.[index].Contains(b.Location.X, b.Location.Y-5.f) then
                    b.Speed <- SizeF(b.Speed.Width, -b.Speed.Height)
            index <- index-1

      let mutable layer = rectdata.Count - 1
      while layer>=0 do
            //prendo il punto di click, lo converto con la trasformazione del layer in uso
            let param1 , rects = rectdata.[layer]
            let mat = param1.VW
            let temp = [| b.Location |]
            let tempTrans = mat.TransformPoints(temp)
            let mutable pos = temp.[0]
            let mutable index = rects.Length - 1
            while index >= 0 do
                //se ne colpisco uno cambio la direzione della palla
                //if rects.[index].IntersectsWith(RectangleF(pos, b.Size)) then
                if rects.[index].Contains(pos) then
                        b.Speed <- SizeF(- b.Speed.Width, - b.Speed.Height)
                        (*if rects.[index].Contains(pos.X+10.f, pos.Y) then
                            b.Speed <- SizeF(- b.Speed.Width, b.Speed.Height)
                        if rects.[index].Contains(pos.X-10.f, pos.Y) then
                            b.Speed <- SizeF(- b.Speed.Width, b.Speed.Height)
                        if rects.[index].Contains(pos.X, pos.Y+10.f) then
                            b.Speed <- SizeF(b.Speed.Width, -b.Speed.Height)
                        if rects.[index].Contains(pos.X, pos.Y-10.f) then
                            b.Speed <- SizeF(b.Speed.Width, -b.Speed.Height)*)
                index <- index-1
            layer <- layer-1
      //controllo collisione con sfere
      b.UpdateSpeed(balls))

  let mutable timer = new Timer(Interval=5)
  do
    timer.Tick.Add(fun _  ->
    updateBalls()
    this.Invalidate())

  let ensurewtransform () =
    let t = 
      match wtransform with 
      | Some(tr) -> tr 
      | None -> let tr = VWCoordinates() in wtransform <- Some(tr); tr
    t
  let t = 
      match wtransform with 
      | Some(tr) -> tr 
      | None -> let tr = VWCoordinates() in wtransform <- Some(tr); tr

  let toRectangle (r:RectangleF) = Rectangle(int r.X, int r.Y, int r.Width, int r.Height)
  let toPointF (p:Point) = PointF(single p.X, single p.Y)

  let mkRectangleF(p1:PointF, p2:PointF) =
    let x1, y1 = min p1.X p2.X, min p1.Y p2.Y
    let x2, y2 = max p1.X p2.X, max p1.Y p2.Y
    RectangleF(x1, y1, x2 - x1, y2 - y1)

  let mutable dragStart = None
  let mutable position = PointF()

  let transformPointVW p = t.TransformPointVW(p)
  //BOTTONI
  let up = RoundButton(Text="Up",Parent=this,Location=PointF(35.f, 0.f), Size=SizeF(35.f, 35.f))
  let left = RoundButton(Text="Left",Parent=this,Location=PointF(0.f, 35.f), Size=SizeF(35.f, 35.f))
  let right = RoundButton(Text="Right",Parent=this,Location=PointF(70.f, 35.f), Size=SizeF(35.f, 35.f))
  let down = RoundButton(Text="Down",Parent=this,Location=PointF(35.f, 70.f), Size=SizeF(35.f, 35.f))

  let pan = RoundButton(Text="Pan",Parent=this,Location=PointF(35.f, 35.f), Size=SizeF(35.f, 35.f))
  let rotLeft = RoundButton(Text="Left",Parent=this,Location=PointF((35.f)/2.f, 106.f), Size=SizeF(35.f, 35.f))
  let rotRight = RoundButton(Text="Right",Parent=this,Location=PointF(35.f+(35.f/2.f)+1.f, 106.f), Size=SizeF(35.f, 35.f))
  let transfR = RoundButton(Text="Zoom+",Parent=this,Location=PointF((35.f)/2.f, 142.f), Size=SizeF(35.f, 35.f))
  let transfL = RoundButton(Text="Zoom-",Parent=this,Location=PointF(35.f+(35.f/2.f)+1.f, 142.f), Size=SizeF(35.f, 35.f))
  let circle = RoundButton(Text="Circle",Parent=this,Location=PointF(110.f, 0.f), Size=SizeF(35.f, 35.f))
  let plus = RoundButton(Text="+",Parent=this,Location=PointF(146.f, 8.f), Size=SizeF(20.f, 20.f))
  let minus = RoundButton(Text="-",Parent=this,Location=PointF(167.f, 8.f), Size=SizeF(20.f, 20.f))
  let box = RoundButton(Text="Box",Parent=this,Location=PointF(110.f, 36.f), Size=SizeF(35.f, 35.f))
  let start = RoundButton(Text="START",Parent=this,Location=PointF(110.f, 107.f), Size=SizeF(35.f, 35.f))
  let delete = RoundButton(Text="DEL",Parent=this,Location=PointF(110.f, 72.f), Size=SizeF(35.f, 35.f))

  let mutable bPressed=false //segnala se è stato prenmuto un tasto
  let mutable animation = false //check avvio/fermata animazione
  do 
    this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
    this.LWControls.AddRange([| up; left; right; down; pan; rotLeft; rotRight; transfR; transfL; circle; plus; minus; box; start; delete |])
    up.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.Translate(0.f, 10.f); up.MouseEventHandled <- true; bPressed<-true; this.Invalidate())
    left.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.Translate(10.f, 0.f); up.MouseEventHandled <- true; bPressed<-true; this.Invalidate())
    right.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.Translate(-10.f, 0.f); up.MouseEventHandled <- true; bPressed<-true; this.Invalidate())
    down.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.Translate(0.f, -10.f); up.MouseEventHandled <- true; bPressed<-true; this.Invalidate())
    rotLeft.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.RotateAt(5.f, Point((this.Width / 2), (this.Height / 2)) |> transformPointVW); this.Invalidate())
    rotRight.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.RotateAt(-5.f, Point((this.Width / 2), (this.Height / 2)) |> transformPointVW); this.Invalidate())
    transfR.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.ScaleAtV(1.1f, 1.1f, Point(this.Width / 2, this.Height / 2)); this.Invalidate())
    transfL.MouseDown.Add(fun _ -> let t = ensurewtransform() in t.ScaleAtV(1.f / 1.1f, 1.f / 1.1f, Point(this.Width / 2, this.Height / 2)); this.Invalidate())

    pan.MouseDown.Add(fun _ -> bPressed<-true; pan.pressed <- not(pan.pressed); match isPan with |true -> isPan<-false |false -> isPan<-true)
    circle.MouseDown.Add(fun _ -> isBox<-false; bPressed<-true)
    plus.MouseDown.Add(fun _ -> if ballsSize < 150.f then ballsSize <- ballsSize + 5.f; bPressed<-true)
    minus.MouseDown.Add(fun _ -> if ballsSize > 5.f then ballsSize <- ballsSize - 5.f; bPressed<-true)
    box.MouseDown.Add(fun _ -> isBox<-true; bPressed<-true)    
    delete.MouseDown.Add(fun _ -> bPressed<-true; delete.pressed <- not(delete.pressed); match isDel with |true -> isDel<-false |false -> isDel<-true)
    start.MouseDown.Add(fun _ -> 
        start.pressed <- not(start.pressed)
        match animation with 
            |true -> 
                animation<-false
                timer.Stop()
            |false ->
                animation<-true
                timer.Start())



  override this.OnPaint e =
    updateBuffer()
    let g = e.Graphics
    let initTransform = g.Transform
    let currentTransform = g.Transform
    
    let layerTransform = 
      (seq { yield match wtransform with Some (t) -> t.Clone() | None -> VWCoordinates() })
      |> Seq.append (rectdata |> Seq.map (fun (wv, _) -> wv.Clone()))
      |> Seq.toArray

    if layerTransform.Length > 1 then
      for i in (layerTransform.Length - 2) .. -1 .. 0 do
        layerTransform.[i].Multiply(layerTransform.[i + 1], order=Drawing2D.MatrixOrder.Append)      

    for i in 0 .. (rectdata.Count - 1) do
      let _, rects = rectdata.[i]
      let wv = layerTransform.[i]
      g.Transform <- wv.WV
      for r in rects do
        g.FillRectangle(Brushes.PaleGreen, r |> toRectangle)
        g.DrawRectangle(Pens.Black, r |> toRectangle)

    g.Transform <- match wtransform with Some(t) -> t.WV | None -> initTransform
    for r in rectangles do
      g.FillRectangle(Brushes.PaleGreen, r |> toRectangle)
      g.DrawRectangle(Pens.Black, r |> toRectangle)

    balls |> Seq.iter (fun b ->
      let r = b.Bounds
      g.FillEllipse(Brushes.Red, r)
      g.DrawEllipse(Pens.DarkRed, r))
    g.DrawLine(Pens.Black, this.Width / 2 - 10, this.Height / 2, this.Width / 2 + 10, this.Height / 2)
    g.DrawLine(Pens.Black, this.Width / 2, this.Height / 2 - 10, this.Width / 2, this.Height / 2 + 10)

    if dragStart.IsSome then
    // se sono in mod rettangolo allora disegna rettangolo
      if isBox && not(isPan) && not(bPressed) && not(isDel) then
        let r = mkRectangleF(dragStart.Value, position) |> toRectangle
        g.DrawRectangle(Pens.Red, r)
    // altrimenti disegna il vettore del cerchio
      elif not(isBox) && not(isPan) && not(bPressed) && not(isDel) then
        let b = new BouncingBall(Size=SizeF(ballsSize, ballsSize), Location=dragStart.Value, Speed=SizeF(0.f,0.f))
        let r = b.Bounds
        g.FillEllipse(Brushes.Red, r)
        g.DrawEllipse(Pens.DarkRed, r)
        g.DrawLine(Pens.Orange, PointF(dragStart.Value.X + ballsSize/2.f,dragStart.Value.Y + ballsSize/2.f), position)



    g.Transform <- initTransform
    base.OnPaint(e)
  // fine paint

  override this.OnMouseDown e =
    base.OnMouseDown e
    if not(this.LWMouseEventHandled) then
      if wtransform.IsSome then
        // se sono in mod rettangolo disegna un rettangolo
        //if isBox && not(isPan) && not(bPressed) && not(isDel) then
            rectdata.Add(wtransform.Value, rectangles |> Seq.toArray)
            wtransform <- None
            rectangles.Clear()
      dragStart <- Some(e.Location |> toPointF)
      position <- e.Location |> toPointF

  override this.OnMouseMove e =
    base.OnMouseMove e
    if not(this.LWMouseEventHandled) && dragStart.IsSome then
      position <- e.Location |> toPointF
      if isPan then
          let t = ensurewtransform() in 
              t.Translate(position.X-dragStart.Value.X, position.Y-dragStart.Value.Y)
          dragStart<-Some(e.Location |> toPointF)
      this.Invalidate()

  override this.OnMouseUp e =
    base.OnMouseUp e
    if not(this.LWMouseEventHandled) && dragStart.IsSome then
      position <- e.Location |> toPointF
      // se sono in mod rettangolo salvo il rettangolo
      if isBox && not(isPan) && not(bPressed) && not(isDel) then
        rectangles.Add(mkRectangleF(dragStart.Value, position))
      // altrimenti salvo il cerchio con il suo vettore
      elif not(isBox) && not(isPan) && not(bPressed) && not(isDel) then
        balls.Add(new BouncingBall(Size=SizeF(ballsSize, ballsSize), Location=PointF(dragStart.Value.X, dragStart.Value.Y), Speed=SizeF(position.X-dragStart.Value.X, position.Y-dragStart.Value.Y)))
      elif isDel then
        let mutable index = rectangles.Count - 1
        let mutable deleted = false
        //scorro l'array delle palline per vedere se ne tocco una
        let mutable i = balls.Count - 1
        while i>=0 && not(deleted) do
            let r = balls.[i].Bounds
            if r.Contains(position) then
                balls.RemoveAt(i)
                deleted<-true
            i <- i - 1
        //scorro l'ultimo layer di rettangoli per vedere se ne tocco uno
        while index >= 0 && not(deleted) do
            if rectangles.[index].Contains(position) then
                rectangles.RemoveAt(index)
                deleted <- not(deleted)
                index <- 0
            index <- index-1
        let mutable layer = rectdata.Count - 1
        for i in (rectdata.Count - 1) .. -1 .. 0 do
        //se non ho trovato un rettangolo da cancellare nel primo strato, proseguo con gli strati successivi, partendo da quello più in rilievo
        while layer>=0 && not(deleted) do
            //prendo il punto di click, lo converto con la matrice di trasformazione del layer in uso
            let param1 , rects = rectdata.[layer]
            let mat = param1.VW
            let temp = [| position |]
            let tempTrans = mat.TransformPoints(temp)
            position <- temp.[0]
            let mutable index = rects.Length - 1
            while index >= 0 && not(deleted) do
                //se ne trovo uno colpito lo elimino, creando un nuovo vettore senza il rettangolo colpito. poi esco dal ciclo
                if rects.[index].Contains(position) then
                    let remove (rects:RectangleF array, index) =
                        let newarray = ResizeArray(rects.Length - 1)
                        let mutable i=0
                        let mutable j=0
                        while i<rects.Length do
                            if i<>index then
                                newarray.Add(rects.[i])
                                j <- j+1
                            i <- i+1
                        (newarray.ToArray())
                    rectdata.[layer] <- (param1, remove(rects, index))
                    deleted <- not(deleted)
                    layer <- 0
                    index <- 0
                index <- index-1
            layer <- layer-1
        
      bPressed <- false
      dragStart <- None
      this.Invalidate()
   
  (*override this.OnKeyDown e =
    let t = 
      match wtransform with 
      | Some(tr) -> tr
      | None -> let tr = VWCoordinates() in wtransform <- Some(tr); tr
    
    match e.KeyData with
    | Keys.W -> let t = ensurewtransform() in t.Translate(0.f, 10.f); this.Invalidate()
    | Keys.S -> let t = ensurewtransform() in t.Translate(0.f, -10.f); this.Invalidate()
    | Keys.A -> let t = ensurewtransform() in t.Translate(10.f, 00.f); this.Invalidate()
    | Keys.D -> let t = ensurewtransform() in t.Translate(-10.f, 0.f); this.Invalidate()
    | Keys.Q -> let t = ensurewtransform() in t.RotateAt(5.f, Point((this.Width / 2), (this.Height / 2)) |> transformPointVW); this.Invalidate()
    | Keys.E -> let t = ensurewtransform() in t.RotateAt(-5.f, Point((this.Width / 2), (this.Height / 2)) |> transformPointVW); this.Invalidate()
    | Keys.Z -> let t = ensurewtransform() in t.ScaleAtV(1.1f, 1.1f, Point(this.Width / 2, this.Height / 2)); this.Invalidate()
    | Keys.X -> let t = ensurewtransform() in t.ScaleAtV(1.f / 1.1f, 1.f / 1.1f, Point(this.Width / 2, this.Height / 2)); this.Invalidate()
    | _ -> ()
    *)
let f = new Form(Text="MidTerm2016",TopMost=true)
f.Show()

let c = new Mid(Dock=DockStyle.Fill)
f.Controls.Add(c)
c.Focus()