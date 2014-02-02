

'///////////////////////////////////////////////////////////////////////////////////////////////////

'#	Copyright (c) 2013, Udayan Roy
'#	All rights reserved.

'#	Redistribution and use in source and binary forms, with or without modification, are
'#	permitted 'provided that the following conditions are met:

'#	* Redistributions of source code must retain the above copyright notice, this list of
'# 	conditions and the following disclaimer.

'#	* Redistributions in binary form must reproduce the above copyright notice, this list of
'# 	 conditions and the following disclaimer in the documentation and/or other materials provided
'#	 with the distribution.

'#	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
'#	IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
'#	AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
'#	CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
'#	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
'#	SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
'#	THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
'#	OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
'#	POSSIBILITY OF SUCH DAMAGE.

'/////////////////////////////////////////////////////////////////////////////////////////////////////



Namespace Geometry


    Public Class GPath
        Private spaths As New List(Of SubPath)

        Private Const fmindist = 0.4


        Public Sub New()

        End Sub

        'Add shapes to the Path.
        'Every shape has added will be a SubPath.

        'Add a subpath of line.
        Public Sub AddLine(ByVal p1 As GPoint, ByVal p2 As GPoint)
            Dim sp As New SubPath

            Dim pp1 As New PathPoint(p1)
            sp.Points.Add(pp1)

            Dim pp2 As New PathPoint(p2)
            sp.Points.Add(pp2)

            Me.spaths.Add(sp)
        End Sub
        Public Sub AddLines(ByVal points() As GPoint)
            Dim sp As New SubPath

            For Each p As GPoint In points
                Dim pp As New PathPoint(p)
                sp.Points.Add(pp)
            Next

            Me.spaths.Add(sp)
        End Sub

        'Add a subpath of Rectangle
        Public Sub AddRectangle(ByVal rect As GRect)
            Me.AddRectangle(rect.X, rect.Y, rect.Width, rect.Height)
        End Sub
        Public Sub AddRectangle(ByVal x As Single, ByVal y As Single, ByVal Width As Single, ByVal Height As Single)
            Dim sp As New SubPath
            sp.Closed = True

            Dim p1 As New PathPoint(New GPoint(x, y))
            sp.Points.Add(p1)
            Dim p2 As New PathPoint(New GPoint(x + Width, y))
            sp.Points.Add(p2)
            Dim p3 As New PathPoint(New GPoint(x + Width, y + Height))
            sp.Points.Add(p3)
            Dim p4 As New PathPoint(New GPoint(x, y + Height))
            sp.Points.Add(p4)

            Me.spaths.Add(sp)
        End Sub
        Public Sub AddRectangle(ByVal p1 As GPoint, ByVal p2 As GPoint)
            Me.AddRectangle(p1.X, p1.Y, p2.X - p1.X, p2.Y - p1.Y)
        End Sub

        'Add a subpath of Ellipse.
        Public Sub AddEllipse(ByVal p1 As GPoint, ByVal p2 As GPoint)
            Me.AddEllipse(p1.X, p1.Y, p2.X - p1.X, p2.Y - p1.Y)
        End Sub
        Public Sub AddEllipse(ByVal x1 As Single, ByVal y1 As Single, ByVal width As Single, ByVal height As Single)

            Dim pathkpa = 0.5522847498
            Dim w = width
            Dim h = height

            Dim w2 = w / 2
            Dim h2 = h / 2

            Dim wk = w2 * pathkpa
            Dim hk = h2 * pathkpa

            Dim sp As New SubPath
            sp.Closed = True

            Dim p1 As New PathPoint(New GPoint(x1 + w2, y1), New GPoint(x1 + w2 - wk, y1), _
                                    New GPoint(x1 + w2 + wk, y1), PathPointType.Smooth)
            sp.Points.Add(p1)
            Dim p2 As New PathPoint(New GPoint(x1 + w, y1 + h2), New GPoint(x1 + w, y1 + h2 - hk), _
                                    New GPoint(x1 + w, y1 + h2 + hk), PathPointType.Smooth)
            sp.Points.Add(p2)
            Dim p3 As New PathPoint(New GPoint(x1 + w2, y1 + h), New GPoint(x1 + w2 + wk, y1 + h), _
                                    New GPoint(x1 + w2 - wk, y1 + h), PathPointType.Smooth)
            sp.Points.Add(p3)
            Dim p4 As New PathPoint(New GPoint(x1, y1 + h2), New GPoint(x1, y1 + h2 + hk), _
                                    New GPoint(x1, y1 + h2 - hk), PathPointType.Smooth)
            sp.Points.Add(p4)

            Me.spaths.Add(sp)
        End Sub
        Public Sub AddEllipse(ByVal rect As GRect)
            Me.AddEllipse(rect.X, rect.Y, rect.Width, rect.Height)
        End Sub

        'Add a SubPath
        Public Sub AddSubPath(ByVal path As SubPath)
            Me.spaths.Add(path)
        End Sub

        Public Function Points() As PathPoint()

            Dim np As Integer = 0

            For Each sp As SubPath In Me.spaths
                np += sp.CountPoints
            Next

            Dim parray As New List(Of PathPoint)(np)

            For Each sp As SubPath In Me.spaths
                parray.AddRange(sp.Points)
            Next
            Return Nothing
        End Function

        Public ReadOnly Property Items(ByVal index As Integer) As SubPath
            Get
                Return Me.spaths.Item(index)
            End Get
        End Property

        Public Function CountItems() As Integer
            Return Me.spaths.Count
        End Function

        Public Sub Transform(ByVal mat As GMatrix)
            For Each sp As SubPath In Me.spaths
                sp.Transform(mat)

            Next
        End Sub

        Public Function GetTightBound() As GRect
            Dim flag As Boolean = True
            Dim bound As GRect
            For Each sp As SubPath In Me.spaths
                If flag Then
                    bound = sp.GetTightBound
                    flag = False
                Else
                    bound = bound.union(sp.GetTightBound)
                End If
            Next
            Return bound
        End Function

        Public Function GetBound() As GRect
            Dim flag As Boolean = True
            Dim bound As GRect
            For Each sp As SubPath In Me.spaths
                If flag Then
                    bound = sp.GetBound
                    flag = False
                Else
                    bound = bound.union(sp.GetBound)
                End If
            Next
            Return bound
        End Function

        Public Function Clone() As GPath
            Dim gp As New GPath
            For Each sp As SubPath In Me.spaths
                gp.spaths.Add(sp.Clone)
            Next
            Return gp
        End Function



        Public Function isOutlineVisible(ByVal p As GPoint, ByVal width As Single) As Boolean
            Dim mindist As Double = Double.MaxValue

            For Each sp As SubPath In Me.spaths
                Dim nb = sp.countBezier
                For i As Integer = 0 To nb - 1
                    Dim bez = sp.getBezier(i)
                    Dim dist = bez.DistancefromPoint(p)
                    mindist = Math.Min(dist, mindist)

                Next
            Next

            Return IIf(mindist <= fmindist, True, False)
        End Function

        Public Function isVisible(ByVal p As GPoint) As Boolean
            Dim nint As Integer = 0
            Dim bound = Me.GetBound
            Dim p1 = bound.Location - New GPoint(20, 20)

            For Each sp As SubPath In Me.spaths
                Dim nb = sp.countBezier
                For i As Integer = 0 To nb - 1
                    Dim bez = sp.getBezier(i)
                    nint += Intersection.intersectBezier3Line(bez.P1, bez.C1, bez.C2, bez.P2, p1, p).nCount()
                Next
            Next

            If nint Mod 2 = 0 Then
                Return False
            Else
                Return True
            End If
        End Function

    End Class

    Public Class SubPath

        Private pts As New List(Of PathPoint)

        Public Sub New()
            Me.Closed = False
        End Sub

        Public ReadOnly Property Points As List(Of PathPoint)
            Get
                Return pts
            End Get

        End Property

        Default Public Property Item(ByVal i As Integer) As PathPoint
            Get
                Return Me.Points(i)
            End Get
            Set(ByVal value As PathPoint)
                Me.Points(i) = value
            End Set
        End Property

        Public Function CountPoints() As Integer
            Return Me.Points.Count
        End Function

        Public Property Closed As Boolean

        Public Sub Transform(ByVal mat As GMatrix)
            For Each pt As PathPoint In Me.Points
                pt.Transform(mat)
            Next
        End Sub

        Public Function GetBound() As GRect

            Dim result As GRect

            Dim n = countBezier()

            result = getBezier(0).GetBound
            If n > 1 Then
                For i As Integer = 1 To n - 1
                    Dim bound = getBezier(i).GetBound
                    result = result.union(bound)
                Next
            End If

            Return result

        End Function

        Public Function GetTightBound() As GRect
            Dim result As GRect


            Dim n = countBezier()

            result = getBezier(0).GetCozyBound
            If n > 1 Then
                For i As Integer = 1 To n - 1
                    Dim bound = getBezier(i).GetCozyBound
                    result = result.union(bound)
                Next
            End If

            Return result
        End Function

        Public Function Clone() As SubPath
            Dim sp As New SubPath
            sp.Closed = Me.Closed

            For Each pt As PathPoint In Me.Points
                sp.Points.Add(pt.Clone)
            Next

            Return sp
        End Function

        Public Function countBezier() As Integer

            Dim n As Integer
            n = CountPoints()
            If Not Closed Then
                If n <> 0 Then n -= 1
            End If
            Return n

        End Function

        Public Function getBezier(ByVal i As Integer) As GCubicBezier
            Dim n = countBezier()
            Dim pp1 = Points(i)

            Dim np2 = i + 1
            Dim npts = CountPoints()

            If np2 = npts Then
                If Closed Then
                    i = 0
                    np2 = 0
                End If
            Else
                np2 = i + 1
            End If
            Dim pp2 = Points(np2)

            Dim c1, c2 As GPoint
            If pp1.Type = PathPointType.None Then
                c1 = pp1.M
            Else
                c1 = pp1.C2
            End If
            If pp2.Type = PathPointType.None Then
                c2 = pp2.M
            Else
                c2 = pp2.C1
            End If
            Dim bez As New GCubicBezier(pp1.M, c1, c2, pp2.M)

            Return bez
        End Function
    End Class





    Public Class PathPoint

        Dim _c1, _c2, _m As GPoint

        Public Sub New()
            _m = New GPoint
            _c1 = New GPoint
            _c2 = New GPoint
        End Sub

        Public Sub New(ByVal p As GPoint)
            _m = p
            _c1 = New GPoint
            _c2 = New GPoint
            Me.Type = PathPointType.None
        End Sub

        Public Sub New(ByVal main As GPoint, ByVal pControlPoint As GPoint, _
                        ByVal nControlPoint As GPoint, Optional ByVal pType As PathPointType = PathPointType.Sharp)

            Me._m = main
            Me._c1 = pControlPoint
            Me._c2 = nControlPoint
            Me.Type = pType

        End Sub

        Public Property M As GPoint
            Get
                Return Me._m
            End Get
            Set(ByVal value As GPoint)
                _m = value
            End Set
        End Property
        Public Property C1 As GPoint
            Get
                Return Me._c1
            End Get
            Set(ByVal value As GPoint)
                Me._c1 = value
            End Set
        End Property
        Public Property C2 As GPoint
            Get
                Return Me._c2
            End Get
            Set(ByVal value As GPoint)
                Me._c2 = value
            End Set
        End Property

        Public Property Type As PathPointType

        Public Sub setValue(ByVal m As GPoint)
            Me.M = m
        End Sub

        Public Sub setValue(ByVal m As GPoint, ByVal c1 As GPoint, ByVal c2 As GPoint)
            Me.M.SetValue(m)
            Me.C1.SetValue(c1)
            Me.C2.SetValue(c2)
        End Sub

        Public Function Clone() As PathPoint
            Dim rp As New PathPoint()
            rp.setValue(Me.M, Me.C1, Me.C2)
            Return rp
        End Function

        Public Sub Transform(ByVal mat As GMatrix)
            mat.map(M)
            mat.map(C1)
            mat.map(C2)

        End Sub
    End Class

    Public Enum PathPointType
        None
        Smooth
        Sharp
    End Enum
End Namespace
