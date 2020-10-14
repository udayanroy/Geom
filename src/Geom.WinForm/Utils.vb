Imports System.Runtime.CompilerServices

Public Module Utils

    <Extension()>
    Public Function Topoint(pt As Drawing.PointF) As Point
        Return New Point(pt.X, pt.Y)
    End Function

    <Extension()>
    Public Function ToPoint(pt As Drawing.Point) As Point
        Return New Point(pt.X, pt.Y)
    End Function

    <Extension()>
    Public Function ToPointf(pt As Point) As Drawing.PointF
        Return New Drawing.PointF(pt.X, pt.Y)
    End Function

    <Extension()>
    Public Function ToRectanglef(pt As Rect) As Drawing.RectangleF
        Return Drawing.Rectangle.FromLTRB(pt.Left, pt.Top, pt.Right, pt.Bottom)
    End Function

    <Extension()>
    Public Function ToRect(pt As Drawing.Rectangle) As Rect
        Return New Rect(pt.Left, pt.Top, pt.Right, pt.Bottom)
    End Function

    <Extension()>
    Friend Function ToGraphicsPath(figure As NodeFigure) As Drawing.Drawing2D.GraphicsPath

        Dim np = figure.CountPoints
        If np < 2 Then
            Return Nothing
            Exit Function
        End If


        Dim lst As New List(Of Drawing.PointF)
        Dim gp As New Drawing.Drawing2D.GraphicsPath()

        Dim p0 = figure.Points(0)

        If p0.Type = NodeType.None Then
            lst.Add(p0.M.ToPointf())
            lst.Add(p0.M.ToPointf)
        Else
            lst.Add(p0.M.ToPointf)
            lst.Add(p0.C2.ToPointf)
        End If

        If np > 2 Then
            For i As Integer = 1 To np - 2
                Dim p As Node = figure.Points(i)

                If p.Type = NodeType.None Then
                    lst.Add(p.M.ToPointf)
                    lst.Add(p.M.ToPointf)
                    lst.Add(p.M.ToPointf)
                Else
                    lst.Add(p.C1.ToPointf)
                    lst.Add(p.M.ToPointf)
                    lst.Add(p.C2.ToPointf)
                End If


            Next
        End If


        Dim pl = figure.Points(np - 1)

        If pl.Type = NodeType.None Then
            lst.Add(pl.M.ToPointf)
            lst.Add(pl.M.ToPointf)
        Else
            lst.Add(pl.C1.ToPointf)
            lst.Add(pl.M.ToPointf)
        End If

        If figure.Closed Then
            lst.Add(pl.C2.ToPointf)

            lst.Add(p0.C1.ToPointf)
            lst.Add(p0.M.ToPointf)

            gp.AddBeziers(lst.ToArray)
            gp.CloseFigure()
        Else
            gp.AddBeziers(lst.ToArray)
            gp.SetMarkers()
        End If




        Return (gp)
    End Function


    <Extension()>
    Friend Function ToGraphicsPath(path As NodePath) As Drawing.Drawing2D.GraphicsPath
        Dim gp As New Drawing.Drawing2D.GraphicsPath

        For Each fig As NodeFigure In path.Figures
            Dim tempGP = fig.ToGraphicsPath
            If tempGP IsNot Nothing Then gp.AddPath(tempGP, False)
        Next

        Return (gp)
    End Function


    <Extension()>
    Friend Function ToGeometry(path As NodePath) As SharpDX.Direct2D1.Geometry
        Dim factory = GraphicsFactory.GetFactory
        Dim geom As New SharpDX.Direct2D1.PathGeometry(factory)

        'geom.
        Dim rct = path.GetBound
        ' Dim rectgeom As New SharpDX.Direct2D1.RectangleGeometry(factory, rct.toRawRectangleF)
        'Try

        'gs.BeginFigure(New Point(0, 0).toRawVector2, FigureBegin.Filled)
        'gs.AddLine(New Point(200, 100).toRawVector2)
        'gs.AddLine(New Point(100, 300).toRawVector2)
        'gs.AddLine(New Point(10, 450).toRawVector2)
        'gs.EndFigure(FigureEnd.Closed)
        'gs.Close()
        'gs.Release()
        'gs.Dispose()
        ' Catch ex As Exception

        'End Try


        ' gs.SetFillMode(FillMode.Winding)
        Dim gs = geom.Open()
        For Each sp In path.Figures

            Dim bezcount = sp.countBezier
            gs.BeginFigure(sp.getBezier(0).P1.toRawVector2, FigureBegin.Filled)
            For i As Integer = 0 To bezcount - 1
                Dim bez = sp.getBezier(i)
                'fig.StartPoint = sp.getBezier(0).P1.ToPoint  // why here??
                gs.AddBezier(bez.toBezierSegment)
            Next

            gs.EndFigure(If(sp.Closed, FigureEnd.Closed, FigureEnd.Open))

        Next
        gs.Close()

        ' Return geom



        Return geom 'rectgeom
    End Function
    <Extension>
    Public Function toBezierSegment(bez As CubicBezier) As BezierSegment

        Dim BZ = New BezierSegment()
        BZ.Point1 = bez.C1.toRawVector2
        BZ.Point2 = bez.C2.toRawVector2
        BZ.Point3 = bez.P2.toRawVector2

        Return BZ
    End Function

    <Extension>
    Public Function toEllipse(rect As Rect) As Ellipse
        Return New Ellipse(rect.midpoint.toRawVector2, rect.Width / 2, rect.Height / 2)
    End Function

    <Extension>
    Public Function toRawRectangleF(rect As Rect) As RawRectangleF
        Return New RawRectangleF(rect.Left, rect.Top, rect.Right, rect.Bottom)
    End Function
    <Extension>
    Public Function toRawRectangle(rect As Rect) As RawRectangle
        Return New RawRectangle(rect.Left, rect.Top, rect.Right, rect.Bottom)
    End Function
    <Extension>
    Public Function toRawVector2(point As Point) As RawVector2
        Return New RawVector2(point.X, point.Y)
    End Function

    <Extension>
    Public Function toGDIpMatrix(mat As Matrix) As Drawing.Drawing2D.Matrix

        Return New Drawing.Drawing2D.Matrix(mat._m11, mat._m12, mat._m21, mat._m22, mat._dx, mat._dy)
    End Function

    <Extension>
    Public Function toD2DMatrix(mat As Matrix) As RawMatrix3x2

        Return New RawMatrix3x2(mat._m11, mat._m12, mat._m21, mat._m22, mat._dx, mat._dy)
    End Function

    <Extension>
    Public Function toGMatrix(mat As RawMatrix3x2) As Matrix

        Return New Matrix(mat.M11, mat.M12, mat.M21, mat.M22, mat.M31, mat.M32)
    End Function


    <Extension>
    Public Function ScaleAt(mat As RawMatrix3x2, Size As Size, center As Point) As RawMatrix3x2

        'https://github.com/apitrace/dxsdk/blob/master/Include/d2d1helper.h

        Dim Scale As New RawMatrix3x2

        Scale.M11 = Size.Width
        Scale.M12 = 0.0
        Scale.M21 = 0.0
        Scale.M22 = Size.Height
        Scale.M31 = center.X - Size.Width * center.X
        Scale.M32 = center.Y - Size.Height * center.Y

        Return Scale

    End Function


    <Extension>
    Public Function Translation(mat As RawMatrix3x2, Size As Size) As RawMatrix3x2

        'https://github.com/apitrace/dxsdk/blob/master/Include/d2d1helper.h

        Dim translationM As New RawMatrix3x2

        translationM.M11 = 1.0
        translationM.M12 = 0.0
        translationM.M21 = 0.0
        translationM.M22 = 1.0

        translationM.M31 = Size.Width
        translationM.M32 = Size.Height

        Return translationM

    End Function

End Module

