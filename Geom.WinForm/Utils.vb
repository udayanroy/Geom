



Imports System.Runtime.CompilerServices
Imports Geom.core

Public Module Utils

    <Extension()>
    Public Function ToGeomPoint(pt As Drawing.PointF) As Geom.core.Point
        Return New Geom.core.Point(pt.X, pt.Y)
    End Function


    <Extension()>
    Public Function ToGeomPoint(pt As Drawing.Point) As Geom.core.Point
        Return New Geom.core.Point(pt.X, pt.Y)
    End Function

    <Extension()>
    Public Function ToPointf(pt As Geom.core.Point) As Drawing.PointF
        Return New Drawing.PointF(pt.X, pt.Y)
    End Function

    <Extension()>
    Public Function ToRectanglef(pt As core.Rect) As Drawing.RectangleF
        Return Drawing.Rectangle.FromLTRB(pt.Left, pt.Top, pt.Right, pt.Bottom)
    End Function



    <Extension()>
    Public Function ToRect(pt As Drawing.Rectangle) As Geom.core.Rect
        Return New core.Rect(pt.Left, pt.Top, pt.Right, pt.Bottom)
    End Function

    <Extension()>
    Friend Function ToGraphicsPath(figure As core.SubPath) As Drawing.Drawing2D.GraphicsPath

        Dim np = figure.CountPoints
        If np < 2 Then
            Return Nothing
            Exit Function
        End If


        Dim lst As New List(Of Drawing.PointF)
        Dim gp As New Drawing.Drawing2D.GraphicsPath()

        Dim p0 = figure.Points(0)

        If p0.Type = PathPointType.None Then
            lst.Add(p0.M.ToPointf())
            lst.Add(p0.M.ToPointf)
        Else
            lst.Add(p0.M.ToPointf)
            lst.Add(p0.C2.ToPointf)
        End If

        If np > 2 Then
            For i As Integer = 1 To np - 2
                Dim p As PathPoint = figure.Points(i)

                If p.Type = PathPointType.None Then
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

        If pl.Type = PathPointType.None Then
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
    Public Function ToGraphicsPath(path As core.GPath) As Drawing.Drawing2D.GraphicsPath
        Dim gp As New Drawing.Drawing2D.GraphicsPath

        For Each fig As core.SubPath In path.Figures
            Dim tempGP = fig.ToGraphicsPath
            If tempGP IsNot Nothing Then gp.AddPath(tempGP, False)
        Next

        Return (gp)
    End Function




    <Extension>
    Public Function toGDIpMatrix(mat As core.GMatrix) As Drawing.Drawing2D.Matrix

        Return New Drawing.Drawing2D.Matrix(mat.M11, mat.M12, mat.M21, mat.M22, mat.dx, mat.dy)
    End Function


    <Extension>
    Public Function ToGeomMatrix(mat As Drawing2D.Matrix) As core.GMatrix

        Return New core.GMatrix(mat.Elements(0), mat.Elements(1),
                                 mat.Elements(2), mat.Elements(3),
                                mat.Elements(4), mat.Elements(5))
    End Function

End Module
