


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
    Public Structure GCubicBezier
        Private _p1, _p2, _c1, _c2 As GPoint




        Public Sub New(ByVal p1 As GPoint, ByVal c1 As GPoint, ByVal c2 As GPoint, ByVal p2 As GPoint)
            _p1 = p1
            _p2 = p2
            _c1 = c1
            _c2 = c2
        End Sub
        Public Property P1() As GPoint
            Get
                Return _p1
            End Get
            Set(ByVal value As GPoint)
                _p1 = value
            End Set
        End Property

        Public Property P2() As GPoint
            Get
                Return _p2
            End Get
            Set(ByVal value As GPoint)
                _p2 = value
            End Set
        End Property

        Public Property C1() As GPoint
            Get
                Return _c1
            End Get
            Set(ByVal value As GPoint)
                _c1 = value
            End Set
        End Property
        Public Property C2() As GPoint
            Get
                Return _c2
            End Get
            Set(ByVal value As GPoint)
                _c2 = value
            End Set
        End Property
        Default Public ReadOnly Property Points(ByVal i As Integer) As GPoint

            Get
                If i = 0 Then
                    Return P1
                ElseIf i = 1 Then
                    Return C1
                ElseIf i = 2 Then
                    Return C2
                ElseIf i = 3 Then
                    Return P2
                Else
                    Return Nothing
                End If
            End Get




        End Property

        Public Sub Transform(ByVal mat As GMatrix)
            mat.map(Me._p1)
            mat.map(Me._p2)
            mat.map(Me._c1)
            mat.map(Me._c2)
        End Sub

        Public Function pointATt(ByVal t As Double) As GPoint
            Dim b As New GPoint
            Dim mt = 1 - t

            b = (mt ^ 3) * _p1 + (3 * (mt ^ 2) * t) * _c1 + (3 * mt * (t ^ 2)) * _c2 + (t ^ 3) * _p2

            Return b
        End Function

        Public Function GetCozyBound() As GRect
            Return Me.computeCubicBoundingBox(P1.X, P1.Y, C1.X, C1.Y, C2.X, C2.Y, P2.X, P2.Y)
        End Function

        Public Function GetBound() As GRect
            Dim r1 As New GRect(P1, P2)
            Dim r2 As New GRect(C1, C2)

            Dim result = r1.union(r2)

            Return result
        End Function

        Private Function computeCubicBaseValue(ByVal t As Single, ByVal a As Single, ByVal b As Single, ByVal c As Single, ByVal d As Single) As Single
            Dim mt = 1 - t
            Return mt * mt * mt * a + 3 * mt * mt * t * b + 3 * mt * t * t * c + t * t * t * d
        End Function
        Private Function computeCubicFirstDerivativeRoots(ByVal a As Single, ByVal b As Single, ByVal c As Single, ByVal d As Single) As Single()

            Dim ret() As Single = {-1, -1}
            Dim tl = -a + 2 * b - c
            Dim tr = -Math.Sqrt(-a * (c - d) + b * b - b * (c + d) + c * c)
            Dim dn = -a + 3 * b - 3 * c + d
            If (dn <> 0) Then

                ret(0) = (tl + tr) / dn
                ret(1) = (tl - tr) / dn
            End If
            Return ret
        End Function
        Private Function computeCubicBoundingBox(ByVal xa As Single, ByVal ya As Single, ByVal xb As Single, ByVal yb As Single, ByVal xc As Single, ByVal yc As Single, ByVal xd As Single, ByVal yd As Single) As GRect

            '// find the zero point for x and y in the derivatives
            Dim minx = 9999
            Dim maxx = -9999
            If (xa < minx) Then minx = xa
            If (xa > maxx) Then maxx = xa
            If (xd < minx) Then minx = xd
            If (xd > maxx) Then maxx = xd
            Dim ts = computeCubicFirstDerivativeRoots(xa, xb, xc, xd)
            For i As Integer = 0 To ts.Length - 1

                Dim t = ts(i)
                If (t >= 0 And t <= 1) Then
                    Dim x = computeCubicBaseValue(t, xa, xb, xc, xd)
                    Dim y = computeCubicBaseValue(t, ya, yb, yc, yd)
                    If (x < minx) Then minx = x
                    If (x > maxx) Then maxx = x

                End If
            Next

            Dim miny = 9999
            Dim maxy = -9999
            If (ya < miny) Then miny = ya
            If (ya > maxy) Then maxy = ya
            If (yd < miny) Then miny = yd
            If (yd > maxy) Then maxy = yd
            ts = computeCubicFirstDerivativeRoots(ya, yb, yc, yd)
            For i = 0 To ts.Length - 1
                Dim t = ts(i)
                If (t >= 0 And t <= 1) Then
                    Dim x = computeCubicBaseValue(t, xa, xb, xc, xd)
                    Dim y = computeCubicBaseValue(t, ya, yb, yc, yd)
                    If (y < miny) Then miny = y
                    If (y > maxy) Then maxy = y
                End If
            Next

            ' // bounding box corner coordinates
            Dim bbox As New GRect
            bbox.point1.SetValue(minx, miny)
            bbox.Point2.SetValue(maxx, maxy)

            Return bbox
        End Function

        Public Function isSelfIntersect(ByRef t1 As Double, ByRef t2 As Double) As Boolean
            Dim a0, a1, a2, a3, b0, b1, b2, b3 As Double
            Dim u2, u1, u0 As Double

            a0 = P1.X
            a1 = 3 * (-P1.X + C1.X)
            a2 = -6 * C1.X + 3 * P1.X + 3 * C2.X
            a3 = P2.X - P1.X - 3 * C2.X + 3 * C1.X

            b0 = P1.Y
            b1 = 3 * (-P1.Y + C1.Y)
            b2 = -6 * C1.Y + 3 * P1.Y + 3 * C2.Y
            b3 = P2.Y - P1.Y - 3 * C2.Y + 3 * C1.Y


            u2 = -2 * a2 * a3 * b2 * b3 + a2 * a2 * b3 * b3 + a3 * a3 * b2 * b2
            u1 = -a1 * a3 * b2 * b3 - a2 * a3 * b1 * b3 + a1 * a2 * b3 * b3 + b1 * b2 * a3 * a3
            u0 = -a1 * a2 * b2 * b3 - a2 * a3 * b1 * b2 - 2 * a1 * a3 * b1 * b3 + a1 * a1 * b3 * b3 _
                  + a3 * a3 * b1 * b1 + a1 * a3 * b2 * b2 + b1 * b3 * a2 * a2



            Dim p As New Polynomial(u2, u1, u0)
            Dim k = p.getRoots()

            Dim s1, s2 As Double
            Dim inter = False

            If k.Count = 2 Then
                s1 = k(0)
                s2 = k(1)

                If s1 > 0 And s1 < 1 And s2 > 0 And s2 < 1 Then
                    inter = True

                    t1 = s1
                    t2 = s2
                End If

            End If



            Return inter
            'If t > 0 And t < 1 Then
            'Return True
            ' Else
            ' Return False
            ' End If


        End Function

        Public Function closestPointToBezier(ByVal p As GPoint) As Single



            Dim d = P1
            Dim c = 3 * (C1 - P1)
            Dim b = -6 * C1 + 3 * P1 + 3 * C2
            Dim a = P2 - P1 - 3 * C2 + 3 * C1


            Dim u5 = 3 * (a * a)
            Dim u4 = 5 * a * b
            Dim u3 = 3 * a * c + 2 * b * b + a * c
            Dim u2 = 3 * (a * d - (a * p) + b * c)
            Dim u1 = 2 * b * d - (2 * b * p) + c * c
            Dim u0 = c * d - (p * c)

            Dim poly As New Polynomial(u5.modulus, u4.modulus, u3.modulus, u2.modulus, u1.modulus, u0.modulus)
            Dim roots = poly.getRootsInInterval(0, 1)


            Dim t As Double
            Dim mindis = Double.MaxValue
            Dim mint As Double = Double.NaN

            Dim dis0 = GPoint.Distance(P1, p)
            Dim dis1 = GPoint.Distance(P2, p)

            For i = 0 To roots.Count - 1
                t = roots(i)

                Dim location = Me.pointATt(t)
                Dim dist = GPoint.Distance(location, p)
                If dist < mindis Then
                    mindis = dist
                    mint = t
                End If


            Next
            If Not (Double.IsNaN(mint)) And mindis < dis0 And mindis < dis1 Then
                Return mint
            Else
                If dis0 < dis1 Then
                    Return 0.0
                Else
                    Return 1.0
                End If
            End If


        End Function
        Public Function DistancefromPoint(ByVal p As GPoint) As Double


            Dim d = P1
            Dim c = 3 * (C1 - P1)
            Dim b = -6 * C1 + 3 * P1 + 3 * C2
            Dim a = P2 - P1 - 3 * C2 + 3 * C1


            Dim u5 = 3 * (a * a)
            Dim u4 = 5 * a * b
            Dim u3 = 3 * a * c + 2 * b * b + a * c
            Dim u2 = 3 * (a * d - (a * p) + b * c)
            Dim u1 = 2 * b * d - (2 * b * p) + c * c
            Dim u0 = c * d - (p * c)

            Dim poly As New Polynomial(u5.modulus, u4.modulus, u3.modulus, u2.modulus, u1.modulus, u0.modulus)
            Dim roots = poly.getRootsInInterval(0, 1)


            Dim t As Double
            Dim mindis = Double.MaxValue
            Dim mint As Double = Double.NaN

            Dim dis0 = GPoint.Distance(P1, p)
            Dim dis1 = GPoint.Distance(P2, p)

            For i = 0 To roots.Count - 1
                t = roots(i)

                Dim location = Me.pointATt(t)
                Dim dist = GPoint.Distance(location, p)
                If dist < mindis Then
                    mindis = dist
                    mint = t
                End If

            Next

            If Not (Double.IsNaN(mint)) And mindis < dis0 And mindis < dis1 Then
                Return mindis
            Else
                If dis0 < dis1 Then
                    Return dis0
                Else
                    Return dis1
                End If
            End If
        End Function
        Public Function divLeft(ByVal t As Double) As GCubicBezier
            Dim r As New GCubicBezier

            Dim dest, ab, bc, cd, abbc, bccd As GPoint
            ab = GPoint.lerp(P1, C1, t)
            bc = GPoint.lerp(C1, C2, t)
            cd = GPoint.lerp(C2, P2, t)
            abbc = GPoint.lerp(ab, bc, t)
            bccd = GPoint.lerp(bc, cd, t)
            dest = GPoint.lerp(abbc, bccd, t)


            r.P1 = P1
            r.C1 = ab
            r.C2 = abbc
            r.P2 = dest

            Return r
        End Function
        Public Function divRight(ByVal t As Double) As GCubicBezier
            Dim r As New GCubicBezier

            Dim dest, ab, bc, cd, abbc, bccd As GPoint
            ab = GPoint.lerp(P1, C1, t)
            bc = GPoint.lerp(C1, C2, t)
            cd = GPoint.lerp(C2, P2, t)
            abbc = GPoint.lerp(ab, bc, t)
            bccd = GPoint.lerp(bc, cd, t)
            dest = GPoint.lerp(abbc, bccd, t)


            r.P1 = P1
            r.C1 = ab
            r.C2 = abbc
            r.P2 = dest

            Return r
        End Function




    End Structure


End Namespace


