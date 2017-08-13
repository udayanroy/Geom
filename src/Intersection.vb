
Namespace Geometry
    Public Class Intersection
        Private nc As Integer
        Private _points As List(Of Point)
        Private status As String


        Private Sub New()
            nc = 0
            _points = New List(Of Point)
        End Sub
        Private Sub New(ByVal status As String)
            nc = 0
            _points = New List(Of Point)
            status = status
        End Sub
        Public Function nCount() As Integer
            Return _points.Count
        End Function
        Public Function Ipoint(ByVal i As Integer) As Point
            If nCount() = 0 Then Return Nothing
            Return _points(i)
        End Function

        Public ReadOnly Property Points As List(Of Point)
            Get
                Return _points
            End Get
        End Property

        Public Function info() As String
            Return status
        End Function

        Private Sub AddPoint(ByVal pnt As Point)
            nc += 1
            _points.Add(pnt)
        End Sub
        Private Sub appendPoints(ByVal pnts As List(Of Point))

            _points.AddRange(pnts)
        End Sub

        Public Shared Function intersectLineLine(ByVal a1 As Point, ByVal a2 As Point, ByVal b1 As Point, ByVal b2 As Point) As Intersection
            Dim result As New Intersection()

            Dim ua_t As Double = (b2.X - b1.X) * (a1.Y - b1.Y) - (b2.Y - b1.Y) * (a1.X - b1.X)
            Dim ub_t As Double = (a2.X - a1.X) * (a1.Y - b1.Y) - (a2.Y - a1.Y) * (a1.X - b1.X)
            Dim u_b As Double = (b2.Y - b1.Y) * (a2.X - a1.X) - (b2.X - b1.X) * (a2.Y - a1.Y)

            If (u_b <> 0) Then
                Dim ua As Double = ua_t / u_b
                Dim ub As Double = ub_t / u_b

                If (0 <= ua And ua <= 1 And 0 <= ub And ub <= 1) Then
                    'result = new Intersection("Intersection");
                    result.AddPoint(New Point(a1.X + ua * (a2.X - a1.X), a1.Y + ua * (a2.Y - a1.Y)))


                    result.status = "No Intersection"
                End If
            Else
                If (ua_t = 0 Or ub_t = 0) Then
                    result.status = "Coincident"
                Else
                    result.status = "Parallel"
                End If
            End If

            Return result
        End Function



        Public Shared Function intersectBezier2Bezier2(ByVal a1 As Point, ByVal a2 As Point, ByVal a3 As Point, ByVal b1 As Point, ByVal b2 As Point, ByVal b3 As Point) As Intersection
            Dim a, b As Point               '// temporary variables
            Dim c12, c11, c10 As Point     '// curve one coefficients
            Dim c22, c21, c20 As Point      '// curve two coefficients
            Dim TOLERANCE = 0.0001   '// determines when two roots are considered equal

            Dim result As New Intersection("No Intersection")

            '// convert from Berstein to quadratic

            '// calculate curve one
            a = (-2) * a2
            c12 = a1 + (a + a3)

            a = a1 * (-2)
            b = a2 * 2
            c11 = a + b

            c10 = New Point(a1.X, a1.Y)

            '// calculate curve two
            a = b2 * (-2)
            c22 = b1 + (a + (b3))

            a = b1 * (-2)
            b = b2 * (2)
            c21 = a + (b)

            c20 = New Point(b1.X, b1.Y)

            '// build sub-expressions used to build polynomial
            ' // these were calculated via a Bezout resultant
            ' //
            '// x1 = x component of the first parametric curve
            '// y1 = y component of the first parametric curve
            '// x2 = x component of the second parametric curve
            '// y2 = y component of the second parametric curve
            '// 
            '// x1 = c12.x*t^2 + c11.x*t + c10.x
            '// y1 = c12.y*t^2 + c11.y*t + c10.y
            '// x2 = c22.x*s^2 + c21.x*s + c20.x
            '// y2 = c22.y*s^2 + c21.y*s + c20.y
            '//
            '// collect the following two equations in terms of t
            '// x2 - x1 = (-c12.x)*t^2 + (-c11.x)*t + (-c10.x + c20.x + c21.x*s + c22.x*s^2)
            '// y2 - y1 = (-c12.y)*t^2 + (-c11.y)*t + (-c10.y + c20.y + c21.y*s + c22.y*s^2)
            '//
            '// We only need to work with the coefficients of these two polynomials
            '// The coefficients are designated above using parentheses
            '//
            '// A = list coeffients of x2 - x1 in terms of t
            '// B = list coeffients of y2 - y1 in terms of t
            '//
            '// Find Bezout Matrix for the two second order polynomials
            '//                /               \
            '//                | v[2,1] v[2,0] |
            '// bezoutMatrix = |               |
            '//                | v[2,0] v[1,0] |
            '//                \               /
            '//
            '// The elements of the Bezout Matrix are defined by the following
            '// v[i,j] = A[i]*B[j] - A[j]*B[i]
            '//
            '// where A[x] and B[x] are defined as
            '// A[x] = coefficient in list A for the monomial of power x
            '// B[x] = coefficient in list B for the monomial of power x
            '//
            '// In this case here, we end up with polynomials with the following
            '// structure
            '//                /                                  \
            '//                |        a         e*s^2 + f*s + g |
            '// bezoutMatrix = |                                  |
            '//                | e*s^2 + f*s + g  b*s^2 + c*s + d |
            '//                \                                  /
            '//
            Dim ad As Double = c12.X * c11.Y - c11.X * c12.Y
            Dim bd = c22.X * c11.Y - c11.X * c22.Y
            Dim c = c21.X * c11.Y - c11.X * c21.Y
            Dim d = c11.X * (c10.Y - c20.Y) + c11.Y * (-c10.X + c20.X)
            Dim e = c22.X * c12.Y - c12.X * c22.Y
            Dim f = c21.X * c12.Y - c12.X * c21.Y
            Dim g = c12.X * (c10.Y - c20.Y) + c12.Y * (-c10.X + c20.X)

            '// The coefficients of this polynomial come from the Bezout resultant which
            '// is the determinant of the bezoutMatrix defined above
            '//
            '//     /     \
            '//     | p q |
            '// Det |     | = ps - qr
            '//     | r s |
            '//     \     /
            '//
            '// Det(bezoutMatrix) = a*(b*s^2 + c*s + d) - (e*s^2 + f*s + g)^2
            '//
            '// When this is expanded and collected in terms of s, you end up with the
            '// polynomial defined here
            '//
            Dim poly = New Polynomial( _
                -e * e, _
                -2 * e * f, _
                ad * bd - f * f - 2 * e * g, _
                ad * c - 2 * f * g, _
                ad * d - g * g _
            )
            Dim roots = poly.getRoots()



            For i As Integer = 0 To roots.Count - 1
                Dim s = roots(i)




                If (0 <= s And s <= 1) Then
                    Dim xRoots = New Polynomial(-c12.X, -c11.X, _
                                -c10.X + c20.X + s * c21.X + s * s * c22.X).getRoots()
                    Dim yRoots = New Polynomial(-c12.Y, -c11.Y, _
                        -c10.Y + c20.Y + s * c21.Y + s * s * c22.Y).getRoots()

                    If (xRoots.Count > 0 And yRoots.Count > 0) Then

                        'checkRoots:
                        For j As Integer = 0 To xRoots.Count - 1
                            Dim xRoot = xRoots(j)

                            If (0 <= xRoot And xRoot <= 1) Then
                                For k As Integer = 0 To yRoots.Count - 1
                                    If (Math.Abs(xRoot - yRoots(k)) < TOLERANCE) Then
                                        result.AddPoint((c22 * (s * s)) + ((c21 * s) + c20))
                                        Exit For
                                    End If
                                Next
                            End If
                        Next
                    End If
                End If
            Next



            Return result
        End Function

        Public Shared Function intersectBezier3Bezier3(ByVal a1 As Point, ByVal a2 As Point, _
                     ByVal a3 As Point, ByVal a4 As Point, ByVal b1 As Point, _
                     ByVal b2 As Point, ByVal b3 As Point, ByVal b4 As Point) As Intersection


            Dim a, b, c, d As Point         '// temporary variables
            Dim c13, c12, c11, c10 As Point '// coefficients of cubic
            Dim c23, c22, c21, c20 As Point '// coefficients of cubic
            Dim result = New Intersection("No Intersection")

            '// Calculate the coefficients of cubic polynomial
            a = a1 * (-1)
            b = a2 * (3)
            c = a3 * (-3)
            d = a + (b + (c + (a4)))
            c13 = New Point(d.X, d.Y)

            a = a1 * (3)
            b = a2 * (-6)
            c = a3 * (3)
            d = a + (b + (c))
            c12 = New Point(d.X, d.Y)

            a = a1 * (-3)
            b = a2 * (3)
            c = a + (b)
            c11 = New Point(c.X, c.Y)

            c10 = New Point(a1.X, a1.Y)

            a = b1 * (-1)
            b = b2 * (3)
            c = b3 * (-3)
            d = a + (b + (c + (b4)))
            c23 = New Point(d.X, d.Y)

            a = b1 * (3)
            b = b2 * (-6)
            c = b3 * (3)
            d = a + (b + (c))
            c22 = New Point(d.X, d.Y)

            a = b1 * (-3)
            b = b2 * (3)
            c = a + (b)
            c21 = New Point(c.X, c.Y)

            c20 = New Point(b1.X, b1.Y)

            Dim c10x2 = c10.X * c10.X
            Dim c10x3 = c10.X * c10.X * c10.X
            Dim c10y2 = c10.Y * c10.Y
            Dim c10y3 = c10.Y * c10.Y * c10.Y
            Dim c11x2 = c11.X * c11.X
            Dim c11x3 = c11.X * c11.X * c11.X
            Dim c11y2 = c11.Y * c11.Y
            Dim c11y3 = c11.Y * c11.Y * c11.Y
            Dim c12x2 = c12.X * c12.X
            Dim c12x3 = c12.X * c12.X * c12.X
            Dim c12y2 = c12.Y * c12.Y
            Dim c12y3 = c12.Y * c12.Y * c12.Y
            Dim c13x2 = c13.X * c13.X
            Dim c13x3 = c13.X * c13.X * c13.X
            Dim c13y2 = c13.Y * c13.Y
            Dim c13y3 = c13.Y * c13.Y * c13.Y
            Dim c20x2 = c20.X * c20.X
            Dim c20x3 = c20.X * c20.X * c20.X
            Dim c20y2 = c20.Y * c20.Y
            Dim c20y3 = c20.Y * c20.Y * c20.Y
            Dim c21x2 = c21.X * c21.X
            Dim c21x3 = c21.X * c21.X * c21.X
            Dim c21y2 = c21.Y * c21.Y
            Dim c22x2 = c22.X * c22.X
            Dim c22x3 = c22.X * c22.X * c22.X
            Dim c22y2 = c22.Y * c22.Y
            Dim c23x2 = c23.X * c23.X
            Dim c23x3 = c23.X * c23.X * c23.X
            Dim c23y2 = c23.Y * c23.Y
            Dim c23y3 = c23.Y * c23.Y * c23.Y
            Dim poly = New Polynomial( _
                -c13x3 * c23y3 + c13y3 * c23x3 - 3 * c13.X * c13y2 * c23x2 * c23.Y + _
                    3 * c13x2 * c13.Y * c23.X * c23y2, _
                -6 * c13.X * c22.X * c13y2 * c23.X * c23.Y + 6 * c13x2 * c13.Y * c22.Y * c23.X * c23.Y + 3 * c22.X * c13y3 * c23x2 - _
                    3 * c13x3 * c22.Y * c23y2 - 3 * c13.X * c13y2 * c22.Y * c23x2 + 3 * c13x2 * c22.X * c13.Y * c23y2, _
                -6 * c21.X * c13.X * c13y2 * c23.X * c23.Y - 6 * c13.X * c22.X * c13y2 * c22.Y * c23.X + 6 * c13x2 * c22.X * c13.Y * c22.Y * c23.Y + _
                    3 * c21.X * c13y3 * c23x2 + 3 * c22x2 * c13y3 * c23.X + 3 * c21.X * c13x2 * c13.Y * c23y2 - 3 * c13.X * c21.Y * c13y2 * c23x2 - _
                    3 * c13.X * c22x2 * c13y2 * c23.Y + c13x2 * c13.Y * c23.X * (6 * c21.Y * c23.Y + 3 * c22y2) + c13x3 * (-c21.Y * c23y2 - _
                    2 * c22y2 * c23.Y - c23.Y * (2 * c21.Y * c23.Y + c22y2)), _
                c11.X * c12.Y * c13.X * c13.Y * c23.X * c23.Y - c11.Y * c12.X * c13.X * c13.Y * c23.X * c23.Y + 6 * c21.X * c22.X * c13y3 * c23.X + _
                    3 * c11.X * c12.X * c13.X * c13.Y * c23y2 + 6 * c10.X * c13.X * c13y2 * c23.X * c23.Y - 3 * c11.X * c12.X * c13y2 * c23.X * c23.Y - _
                    3 * c11.Y * c12.Y * c13.X * c13.Y * c23x2 - 6 * c10.Y * c13x2 * c13.Y * c23.X * c23.Y - 6 * c20.X * c13.X * c13y2 * c23.X * c23.Y + _
                    3 * c11.Y * c12.Y * c13x2 * c23.X * c23.Y - 2 * c12.X * c12y2 * c13.X * c23.X * c23.Y - 6 * c21.X * c13.X * c22.X * c13y2 * c23.Y - _
                    6 * c21.X * c13.X * c13y2 * c22.Y * c23.X - 6 * c13.X * c21.Y * c22.X * c13y2 * c23.X + 6 * c21.X * c13x2 * c13.Y * c22.Y * c23.Y + _
                    2 * c12x2 * c12.Y * c13.Y * c23.X * c23.Y + c22x3 * c13y3 - 3 * c10.X * c13y3 * c23x2 + 3 * c10.Y * c13x3 * c23y2 + _
                    3 * c20.X * c13y3 * c23x2 + c12y3 * c13.X * c23x2 - c12x3 * c13.Y * c23y2 - 3 * c10.X * c13x2 * c13.Y * c23y2 + _
                    3 * c10.Y * c13.X * c13y2 * c23x2 - 2 * c11.X * c12.Y * c13x2 * c23y2 + c11.X * c12.Y * c13y2 * c23x2 - c11.Y * c12.X * c13x2 * c23y2 + _
                    2 * c11.Y * c12.X * c13y2 * c23x2 + 3 * c20.X * c13x2 * c13.Y * c23y2 - c12.X * c12y2 * c13.Y * c23x2 - _
                    3 * c20.Y * c13.X * c13y2 * c23x2 + c12x2 * c12.Y * c13.X * c23y2 - 3 * c13.X * c22x2 * c13y2 * c22.Y + _
                    c13x2 * c13.Y * c23.X * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c13x2 * c22.X * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) + _
                    c13x3 * (-2 * c21.Y * c22.Y * c23.Y - c20.Y * c23y2 - c22.Y * (2 * c21.Y * c23.Y + c22y2) - c23.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)), _
                6 * c11.X * c12.X * c13.X * c13.Y * c22.Y * c23.Y + c11.X * c12.Y * c13.X * c22.X * c13.Y * c23.Y + c11.X * c12.Y * c13.X * c13.Y * c22.Y * c23.X - _
                    c11.Y * c12.X * c13.X * c22.X * c13.Y * c23.Y - c11.Y * c12.X * c13.X * c13.Y * c22.Y * c23.X - 6 * c11.Y * c12.Y * c13.X * c22.X * c13.Y * c23.X - _
                    6 * c10.X * c22.X * c13y3 * c23.X + 6 * c20.X * c22.X * c13y3 * c23.X + 6 * c10.Y * c13x3 * c22.Y * c23.Y + 2 * c12y3 * c13.X * c22.X * c23.X - _
                    2 * c12x3 * c13.Y * c22.Y * c23.Y + 6 * c10.X * c13.X * c22.X * c13y2 * c23.Y + 6 * c10.X * c13.X * c13y2 * c22.Y * c23.X + _
                    6 * c10.Y * c13.X * c22.X * c13y2 * c23.X - 3 * c11.X * c12.X * c22.X * c13y2 * c23.Y - 3 * c11.X * c12.X * c13y2 * c22.Y * c23.X + _
                    2 * c11.X * c12.Y * c22.X * c13y2 * c23.X + 4 * c11.Y * c12.X * c22.X * c13y2 * c23.X - 6 * c10.X * c13x2 * c13.Y * c22.Y * c23.Y - _
                    6 * c10.Y * c13x2 * c22.X * c13.Y * c23.Y - 6 * c10.Y * c13x2 * c13.Y * c22.Y * c23.X - 4 * c11.X * c12.Y * c13x2 * c22.Y * c23.Y - _
                    6 * c20.X * c13.X * c22.X * c13y2 * c23.Y - 6 * c20.X * c13.X * c13y2 * c22.Y * c23.X - 2 * c11.Y * c12.X * c13x2 * c22.Y * c23.Y + _
                    3 * c11.Y * c12.Y * c13x2 * c22.X * c23.Y + 3 * c11.Y * c12.Y * c13x2 * c22.Y * c23.X - 2 * c12.X * c12y2 * c13.X * c22.X * c23.Y - _
                    2 * c12.X * c12y2 * c13.X * c22.Y * c23.X - 2 * c12.X * c12y2 * c22.X * c13.Y * c23.X - 6 * c20.Y * c13.X * c22.X * c13y2 * c23.X - _
                    6 * c21.X * c13.X * c21.Y * c13y2 * c23.X - 6 * c21.X * c13.X * c22.X * c13y2 * c22.Y + 6 * c20.X * c13x2 * c13.Y * c22.Y * c23.Y + _
                    2 * c12x2 * c12.Y * c13.X * c22.Y * c23.Y + 2 * c12x2 * c12.Y * c22.X * c13.Y * c23.Y + 2 * c12x2 * c12.Y * c13.Y * c22.Y * c23.X + _
                    3 * c21.X * c22x2 * c13y3 + 3 * c21x2 * c13y3 * c23.X - 3 * c13.X * c21.Y * c22x2 * c13y2 - 3 * c21x2 * c13.X * c13y2 * c23.Y + _
                    c13x2 * c22.X * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c13x2 * c13.Y * c23.X * (6 * c20.Y * c22.Y + 3 * c21y2) + _
                    c21.X * c13x2 * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) + c13x3 * (-2 * c20.Y * c22.Y * c23.Y - c23.Y * (2 * c20.Y * c22.Y + c21y2) - _
                    c21.Y * (2 * c21.Y * c23.Y + c22y2) - c22.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)), _
                c11.X * c21.X * c12.Y * c13.X * c13.Y * c23.Y + c11.X * c12.Y * c13.X * c21.Y * c13.Y * c23.X + c11.X * c12.Y * c13.X * c22.X * c13.Y * c22.Y - _
                    c11.Y * c12.X * c21.X * c13.X * c13.Y * c23.Y - c11.Y * c12.X * c13.X * c21.Y * c13.Y * c23.X - c11.Y * c12.X * c13.X * c22.X * c13.Y * c22.Y - _
                    6 * c11.Y * c21.X * c12.Y * c13.X * c13.Y * c23.X - 6 * c10.X * c21.X * c13y3 * c23.X + 6 * c20.X * c21.X * c13y3 * c23.X + _
                    2 * c21.X * c12y3 * c13.X * c23.X + 6 * c10.X * c21.X * c13.X * c13y2 * c23.Y + 6 * c10.X * c13.X * c21.Y * c13y2 * c23.X + _
                    6 * c10.X * c13.X * c22.X * c13y2 * c22.Y + 6 * c10.Y * c21.X * c13.X * c13y2 * c23.X - 3 * c11.X * c12.X * c21.X * c13y2 * c23.Y - _
                    3 * c11.X * c12.X * c21.Y * c13y2 * c23.X - 3 * c11.X * c12.X * c22.X * c13y2 * c22.Y + 2 * c11.X * c21.X * c12.Y * c13y2 * c23.X + _
                    4 * c11.Y * c12.X * c21.X * c13y2 * c23.X - 6 * c10.Y * c21.X * c13x2 * c13.Y * c23.Y - 6 * c10.Y * c13x2 * c21.Y * c13.Y * c23.X - _
                    6 * c10.Y * c13x2 * c22.X * c13.Y * c22.Y - 6 * c20.X * c21.X * c13.X * c13y2 * c23.Y - 6 * c20.X * c13.X * c21.Y * c13y2 * c23.X - _
                    6 * c20.X * c13.X * c22.X * c13y2 * c22.Y + 3 * c11.Y * c21.X * c12.Y * c13x2 * c23.Y - 3 * c11.Y * c12.Y * c13.X * c22x2 * c13.Y + _
                    3 * c11.Y * c12.Y * c13x2 * c21.Y * c23.X + 3 * c11.Y * c12.Y * c13x2 * c22.X * c22.Y - 2 * c12.X * c21.X * c12y2 * c13.X * c23.Y - _
                    2 * c12.X * c21.X * c12y2 * c13.Y * c23.X - 2 * c12.X * c12y2 * c13.X * c21.Y * c23.X - 2 * c12.X * c12y2 * c13.X * c22.X * c22.Y - _
                    6 * c20.Y * c21.X * c13.X * c13y2 * c23.X - 6 * c21.X * c13.X * c21.Y * c22.X * c13y2 + 6 * c20.Y * c13x2 * c21.Y * c13.Y * c23.X + _
                    2 * c12x2 * c21.X * c12.Y * c13.Y * c23.Y + 2 * c12x2 * c12.Y * c21.Y * c13.Y * c23.X + 2 * c12x2 * c12.Y * c22.X * c13.Y * c22.Y - _
                    3 * c10.X * c22x2 * c13y3 + 3 * c20.X * c22x2 * c13y3 + 3 * c21x2 * c22.X * c13y3 + c12y3 * c13.X * c22x2 + _
                    3 * c10.Y * c13.X * c22x2 * c13y2 + c11.X * c12.Y * c22x2 * c13y2 + 2 * c11.Y * c12.X * c22x2 * c13y2 - _
                    c12.X * c12y2 * c22x2 * c13.Y - 3 * c20.Y * c13.X * c22x2 * c13y2 - 3 * c21x2 * c13.X * c13y2 * c22.Y + _
                    c12x2 * c12.Y * c13.X * (2 * c21.Y * c23.Y + c22y2) + c11.X * c12.X * c13.X * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) + _
                    c21.X * c13x2 * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c12x3 * c13.Y * (-2 * c21.Y * c23.Y - c22y2) + _
                    c10.Y * c13x3 * (6 * c21.Y * c23.Y + 3 * c22y2) + c11.Y * c12.X * c13x2 * (-2 * c21.Y * c23.Y - c22y2) + _
                    c11.X * c12.Y * c13x2 * (-4 * c21.Y * c23.Y - 2 * c22y2) + c10.X * c13x2 * c13.Y * (-6 * c21.Y * c23.Y - 3 * c22y2) + _
                    c13x2 * c22.X * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) + c20.X * c13x2 * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) + _
                    c13x3 * (-2 * c20.Y * c21.Y * c23.Y - c22.Y * (2 * c20.Y * c22.Y + c21y2) - c20.Y * (2 * c21.Y * c23.Y + c22y2) - _
                    c21.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)), _
                -c10.X * c11.X * c12.Y * c13.X * c13.Y * c23.Y + c10.X * c11.Y * c12.X * c13.X * c13.Y * c23.Y + 6 * c10.X * c11.Y * c12.Y * c13.X * c13.Y * c23.X - _
                    6 * c10.Y * c11.X * c12.X * c13.X * c13.Y * c23.Y - c10.Y * c11.X * c12.Y * c13.X * c13.Y * c23.X + c10.Y * c11.Y * c12.X * c13.X * c13.Y * c23.X + _
                    c11.X * c11.Y * c12.X * c12.Y * c13.X * c23.Y - c11.X * c11.Y * c12.X * c12.Y * c13.Y * c23.X + c11.X * c20.X * c12.Y * c13.X * c13.Y * c23.Y + _
                    c11.X * c20.Y * c12.Y * c13.X * c13.Y * c23.X + c11.X * c21.X * c12.Y * c13.X * c13.Y * c22.Y + c11.X * c12.Y * c13.X * c21.Y * c22.X * c13.Y - _
                    c20.X * c11.Y * c12.X * c13.X * c13.Y * c23.Y - 6 * c20.X * c11.Y * c12.Y * c13.X * c13.Y * c23.X - c11.Y * c12.X * c20.Y * c13.X * c13.Y * c23.X - _
                    c11.Y * c12.X * c21.X * c13.X * c13.Y * c22.Y - c11.Y * c12.X * c13.X * c21.Y * c22.X * c13.Y - 6 * c11.Y * c21.X * c12.Y * c13.X * c22.X * c13.Y - _
                    6 * c10.X * c20.X * c13y3 * c23.X - 6 * c10.X * c21.X * c22.X * c13y3 - 2 * c10.X * c12y3 * c13.X * c23.X + 6 * c20.X * c21.X * c22.X * c13y3 + _
                    2 * c20.X * c12y3 * c13.X * c23.X + 2 * c21.X * c12y3 * c13.X * c22.X + 2 * c10.Y * c12x3 * c13.Y * c23.Y - 6 * c10.X * c10.Y * c13.X * c13y2 * c23.X + _
                    3 * c10.X * c11.X * c12.X * c13y2 * c23.Y - 2 * c10.X * c11.X * c12.Y * c13y2 * c23.X - 4 * c10.X * c11.Y * c12.X * c13y2 * c23.X + _
                    3 * c10.Y * c11.X * c12.X * c13y2 * c23.X + 6 * c10.X * c10.Y * c13x2 * c13.Y * c23.Y + 6 * c10.X * c20.X * c13.X * c13y2 * c23.Y - _
                    3 * c10.X * c11.Y * c12.Y * c13x2 * c23.Y + 2 * c10.X * c12.X * c12y2 * c13.X * c23.Y + 2 * c10.X * c12.X * c12y2 * c13.Y * c23.X + _
                    6 * c10.X * c20.Y * c13.X * c13y2 * c23.X + 6 * c10.X * c21.X * c13.X * c13y2 * c22.Y + 6 * c10.X * c13.X * c21.Y * c22.X * c13y2 + _
                    4 * c10.Y * c11.X * c12.Y * c13x2 * c23.Y + 6 * c10.Y * c20.X * c13.X * c13y2 * c23.X + 2 * c10.Y * c11.Y * c12.X * c13x2 * c23.Y - _
                    3 * c10.Y * c11.Y * c12.Y * c13x2 * c23.X + 2 * c10.Y * c12.X * c12y2 * c13.X * c23.X + 6 * c10.Y * c21.X * c13.X * c22.X * c13y2 - _
                    3 * c11.X * c20.X * c12.X * c13y2 * c23.Y + 2 * c11.X * c20.X * c12.Y * c13y2 * c23.X + c11.X * c11.Y * c12y2 * c13.X * c23.X - _
                    3 * c11.X * c12.X * c20.Y * c13y2 * c23.X - 3 * c11.X * c12.X * c21.X * c13y2 * c22.Y - 3 * c11.X * c12.X * c21.Y * c22.X * c13y2 + _
                    2 * c11.X * c21.X * c12.Y * c22.X * c13y2 + 4 * c20.X * c11.Y * c12.X * c13y2 * c23.X + 4 * c11.Y * c12.X * c21.X * c22.X * c13y2 - _
                    2 * c10.X * c12x2 * c12.Y * c13.Y * c23.Y - 6 * c10.Y * c20.X * c13x2 * c13.Y * c23.Y - 6 * c10.Y * c20.Y * c13x2 * c13.Y * c23.X - _
                    6 * c10.Y * c21.X * c13x2 * c13.Y * c22.Y - 2 * c10.Y * c12x2 * c12.Y * c13.X * c23.Y - 2 * c10.Y * c12x2 * c12.Y * c13.Y * c23.X - _
                    6 * c10.Y * c13x2 * c21.Y * c22.X * c13.Y - c11.X * c11.Y * c12x2 * c13.Y * c23.Y - 2 * c11.X * c11y2 * c13.X * c13.Y * c23.X + _
                    3 * c20.X * c11.Y * c12.Y * c13x2 * c23.Y - 2 * c20.X * c12.X * c12y2 * c13.X * c23.Y - 2 * c20.X * c12.X * c12y2 * c13.Y * c23.X - _
                    6 * c20.X * c20.Y * c13.X * c13y2 * c23.X - 6 * c20.X * c21.X * c13.X * c13y2 * c22.Y - 6 * c20.X * c13.X * c21.Y * c22.X * c13y2 + _
                    3 * c11.Y * c20.Y * c12.Y * c13x2 * c23.X + 3 * c11.Y * c21.X * c12.Y * c13x2 * c22.Y + 3 * c11.Y * c12.Y * c13x2 * c21.Y * c22.X - _
                    2 * c12.X * c20.Y * c12y2 * c13.X * c23.X - 2 * c12.X * c21.X * c12y2 * c13.X * c22.Y - 2 * c12.X * c21.X * c12y2 * c22.X * c13.Y - _
                    2 * c12.X * c12y2 * c13.X * c21.Y * c22.X - 6 * c20.Y * c21.X * c13.X * c22.X * c13y2 - c11y2 * c12.X * c12.Y * c13.X * c23.X + _
                    2 * c20.X * c12x2 * c12.Y * c13.Y * c23.Y + 6 * c20.Y * c13x2 * c21.Y * c22.X * c13.Y + 2 * c11x2 * c11.Y * c13.X * c13.Y * c23.Y + _
                    c11x2 * c12.X * c12.Y * c13.Y * c23.Y + 2 * c12x2 * c20.Y * c12.Y * c13.Y * c23.X + 2 * c12x2 * c21.X * c12.Y * c13.Y * c22.Y + _
                    2 * c12x2 * c12.Y * c21.Y * c22.X * c13.Y + c21x3 * c13y3 + 3 * c10x2 * c13y3 * c23.X - 3 * c10y2 * c13x3 * c23.Y + _
                    3 * c20x2 * c13y3 * c23.X + c11y3 * c13x2 * c23.X - c11x3 * c13y2 * c23.Y - c11.X * c11y2 * c13x2 * c23.Y + _
                    c11x2 * c11.Y * c13y2 * c23.X - 3 * c10x2 * c13.X * c13y2 * c23.Y + 3 * c10y2 * c13x2 * c13.Y * c23.X - c11x2 * c12y2 * c13.X * c23.Y + _
                    c11y2 * c12x2 * c13.Y * c23.X - 3 * c21x2 * c13.X * c21.Y * c13y2 - 3 * c20x2 * c13.X * c13y2 * c23.Y + 3 * c20y2 * c13x2 * c13.Y * c23.X + _
                    c11.X * c12.X * c13.X * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c12x3 * c13.Y * (-2 * c20.Y * c23.Y - 2 * c21.Y * c22.Y) + _
                    c10.Y * c13x3 * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c11.Y * c12.X * c13x2 * (-2 * c20.Y * c23.Y - 2 * c21.Y * c22.Y) + _
                    c12x2 * c12.Y * c13.X * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y) + c11.X * c12.Y * c13x2 * (-4 * c20.Y * c23.Y - 4 * c21.Y * c22.Y) + _
                    c10.X * c13x2 * c13.Y * (-6 * c20.Y * c23.Y - 6 * c21.Y * c22.Y) + c20.X * c13x2 * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + _
                    c21.X * c13x2 * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) + c13x3 * (-2 * c20.Y * c21.Y * c22.Y - c20y2 * c23.Y - _
                    c21.Y * (2 * c20.Y * c22.Y + c21y2) - c20.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)), _
                -c10.X * c11.X * c12.Y * c13.X * c13.Y * c22.Y + c10.X * c11.Y * c12.X * c13.X * c13.Y * c22.Y + 6 * c10.X * c11.Y * c12.Y * c13.X * c22.X * c13.Y - _
                    6 * c10.Y * c11.X * c12.X * c13.X * c13.Y * c22.Y - c10.Y * c11.X * c12.Y * c13.X * c22.X * c13.Y + c10.Y * c11.Y * c12.X * c13.X * c22.X * c13.Y + _
                    c11.X * c11.Y * c12.X * c12.Y * c13.X * c22.Y - c11.X * c11.Y * c12.X * c12.Y * c22.X * c13.Y + c11.X * c20.X * c12.Y * c13.X * c13.Y * c22.Y + _
                    c11.X * c20.Y * c12.Y * c13.X * c22.X * c13.Y + c11.X * c21.X * c12.Y * c13.X * c21.Y * c13.Y - c20.X * c11.Y * c12.X * c13.X * c13.Y * c22.Y - _
                    6 * c20.X * c11.Y * c12.Y * c13.X * c22.X * c13.Y - c11.Y * c12.X * c20.Y * c13.X * c22.X * c13.Y - c11.Y * c12.X * c21.X * c13.X * c21.Y * c13.Y - _
                    6 * c10.X * c20.X * c22.X * c13y3 - 2 * c10.X * c12y3 * c13.X * c22.X + 2 * c20.X * c12y3 * c13.X * c22.X + 2 * c10.Y * c12x3 * c13.Y * c22.Y - _
                    6 * c10.X * c10.Y * c13.X * c22.X * c13y2 + 3 * c10.X * c11.X * c12.X * c13y2 * c22.Y - 2 * c10.X * c11.X * c12.Y * c22.X * c13y2 - _
                    4 * c10.X * c11.Y * c12.X * c22.X * c13y2 + 3 * c10.Y * c11.X * c12.X * c22.X * c13y2 + 6 * c10.X * c10.Y * c13x2 * c13.Y * c22.Y + _
                    6 * c10.X * c20.X * c13.X * c13y2 * c22.Y - 3 * c10.X * c11.Y * c12.Y * c13x2 * c22.Y + 2 * c10.X * c12.X * c12y2 * c13.X * c22.Y + _
                    2 * c10.X * c12.X * c12y2 * c22.X * c13.Y + 6 * c10.X * c20.Y * c13.X * c22.X * c13y2 + 6 * c10.X * c21.X * c13.X * c21.Y * c13y2 + _
                    4 * c10.Y * c11.X * c12.Y * c13x2 * c22.Y + 6 * c10.Y * c20.X * c13.X * c22.X * c13y2 + 2 * c10.Y * c11.Y * c12.X * c13x2 * c22.Y - _
                    3 * c10.Y * c11.Y * c12.Y * c13x2 * c22.X + 2 * c10.Y * c12.X * c12y2 * c13.X * c22.X - 3 * c11.X * c20.X * c12.X * c13y2 * c22.Y + _
                    2 * c11.X * c20.X * c12.Y * c22.X * c13y2 + c11.X * c11.Y * c12y2 * c13.X * c22.X - 3 * c11.X * c12.X * c20.Y * c22.X * c13y2 - _
                    3 * c11.X * c12.X * c21.X * c21.Y * c13y2 + 4 * c20.X * c11.Y * c12.X * c22.X * c13y2 - 2 * c10.X * c12x2 * c12.Y * c13.Y * c22.Y - _
                    6 * c10.Y * c20.X * c13x2 * c13.Y * c22.Y - 6 * c10.Y * c20.Y * c13x2 * c22.X * c13.Y - 6 * c10.Y * c21.X * c13x2 * c21.Y * c13.Y - _
                    2 * c10.Y * c12x2 * c12.Y * c13.X * c22.Y - 2 * c10.Y * c12x2 * c12.Y * c22.X * c13.Y - c11.X * c11.Y * c12x2 * c13.Y * c22.Y - _
                    2 * c11.X * c11y2 * c13.X * c22.X * c13.Y + 3 * c20.X * c11.Y * c12.Y * c13x2 * c22.Y - 2 * c20.X * c12.X * c12y2 * c13.X * c22.Y - _
                    2 * c20.X * c12.X * c12y2 * c22.X * c13.Y - 6 * c20.X * c20.Y * c13.X * c22.X * c13y2 - 6 * c20.X * c21.X * c13.X * c21.Y * c13y2 + _
                    3 * c11.Y * c20.Y * c12.Y * c13x2 * c22.X + 3 * c11.Y * c21.X * c12.Y * c13x2 * c21.Y - 2 * c12.X * c20.Y * c12y2 * c13.X * c22.X - _
                    2 * c12.X * c21.X * c12y2 * c13.X * c21.Y - c11y2 * c12.X * c12.Y * c13.X * c22.X + 2 * c20.X * c12x2 * c12.Y * c13.Y * c22.Y - _
                    3 * c11.Y * c21x2 * c12.Y * c13.X * c13.Y + 6 * c20.Y * c21.X * c13x2 * c21.Y * c13.Y + 2 * c11x2 * c11.Y * c13.X * c13.Y * c22.Y + _
                    c11x2 * c12.X * c12.Y * c13.Y * c22.Y + 2 * c12x2 * c20.Y * c12.Y * c22.X * c13.Y + 2 * c12x2 * c21.X * c12.Y * c21.Y * c13.Y - _
                    3 * c10.X * c21x2 * c13y3 + 3 * c20.X * c21x2 * c13y3 + 3 * c10x2 * c22.X * c13y3 - 3 * c10y2 * c13x3 * c22.Y + 3 * c20x2 * c22.X * c13y3 + _
                    c21x2 * c12y3 * c13.X + c11y3 * c13x2 * c22.X - c11x3 * c13y2 * c22.Y + 3 * c10.Y * c21x2 * c13.X * c13y2 - _
                    c11.X * c11y2 * c13x2 * c22.Y + c11.X * c21x2 * c12.Y * c13y2 + 2 * c11.Y * c12.X * c21x2 * c13y2 + c11x2 * c11.Y * c22.X * c13y2 - _
                    c12.X * c21x2 * c12y2 * c13.Y - 3 * c20.Y * c21x2 * c13.X * c13y2 - 3 * c10x2 * c13.X * c13y2 * c22.Y + 3 * c10y2 * c13x2 * c22.X * c13.Y - _
                    c11x2 * c12y2 * c13.X * c22.Y + c11y2 * c12x2 * c22.X * c13.Y - 3 * c20x2 * c13.X * c13y2 * c22.Y + 3 * c20y2 * c13x2 * c22.X * c13.Y + _
                    c12x2 * c12.Y * c13.X * (2 * c20.Y * c22.Y + c21y2) + c11.X * c12.X * c13.X * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) + _
                    c12x3 * c13.Y * (-2 * c20.Y * c22.Y - c21y2) + c10.Y * c13x3 * (6 * c20.Y * c22.Y + 3 * c21y2) + _
                    c11.Y * c12.X * c13x2 * (-2 * c20.Y * c22.Y - c21y2) + c11.X * c12.Y * c13x2 * (-4 * c20.Y * c22.Y - 2 * c21y2) + _
                    c10.X * c13x2 * c13.Y * (-6 * c20.Y * c22.Y - 3 * c21y2) + c20.X * c13x2 * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) + _
                    c13x3 * (-2 * c20.Y * c21y2 - c20y2 * c22.Y - c20.Y * (2 * c20.Y * c22.Y + c21y2)), _
                -c10.X * c11.X * c12.Y * c13.X * c21.Y * c13.Y + c10.X * c11.Y * c12.X * c13.X * c21.Y * c13.Y + 6 * c10.X * c11.Y * c21.X * c12.Y * c13.X * c13.Y - _
                    6 * c10.Y * c11.X * c12.X * c13.X * c21.Y * c13.Y - c10.Y * c11.X * c21.X * c12.Y * c13.X * c13.Y + c10.Y * c11.Y * c12.X * c21.X * c13.X * c13.Y - _
                    c11.X * c11.Y * c12.X * c21.X * c12.Y * c13.Y + c11.X * c11.Y * c12.X * c12.Y * c13.X * c21.Y + c11.X * c20.X * c12.Y * c13.X * c21.Y * c13.Y + _
                    6 * c11.X * c12.X * c20.Y * c13.X * c21.Y * c13.Y + c11.X * c20.Y * c21.X * c12.Y * c13.X * c13.Y - c20.X * c11.Y * c12.X * c13.X * c21.Y * c13.Y - _
                    6 * c20.X * c11.Y * c21.X * c12.Y * c13.X * c13.Y - c11.Y * c12.X * c20.Y * c21.X * c13.X * c13.Y - 6 * c10.X * c20.X * c21.X * c13y3 - _
                    2 * c10.X * c21.X * c12y3 * c13.X + 6 * c10.Y * c20.Y * c13x3 * c21.Y + 2 * c20.X * c21.X * c12y3 * c13.X + 2 * c10.Y * c12x3 * c21.Y * c13.Y - _
                    2 * c12x3 * c20.Y * c21.Y * c13.Y - 6 * c10.X * c10.Y * c21.X * c13.X * c13y2 + 3 * c10.X * c11.X * c12.X * c21.Y * c13y2 - _
                    2 * c10.X * c11.X * c21.X * c12.Y * c13y2 - 4 * c10.X * c11.Y * c12.X * c21.X * c13y2 + 3 * c10.Y * c11.X * c12.X * c21.X * c13y2 + _
                    6 * c10.X * c10.Y * c13x2 * c21.Y * c13.Y + 6 * c10.X * c20.X * c13.X * c21.Y * c13y2 - 3 * c10.X * c11.Y * c12.Y * c13x2 * c21.Y + _
                    2 * c10.X * c12.X * c21.X * c12y2 * c13.Y + 2 * c10.X * c12.X * c12y2 * c13.X * c21.Y + 6 * c10.X * c20.Y * c21.X * c13.X * c13y2 + _
                    4 * c10.Y * c11.X * c12.Y * c13x2 * c21.Y + 6 * c10.Y * c20.X * c21.X * c13.X * c13y2 + 2 * c10.Y * c11.Y * c12.X * c13x2 * c21.Y - _
                    3 * c10.Y * c11.Y * c21.X * c12.Y * c13x2 + 2 * c10.Y * c12.X * c21.X * c12y2 * c13.X - 3 * c11.X * c20.X * c12.X * c21.Y * c13y2 + _
                    2 * c11.X * c20.X * c21.X * c12.Y * c13y2 + c11.X * c11.Y * c21.X * c12y2 * c13.X - 3 * c11.X * c12.X * c20.Y * c21.X * c13y2 + _
                    4 * c20.X * c11.Y * c12.X * c21.X * c13y2 - 6 * c10.X * c20.Y * c13x2 * c21.Y * c13.Y - 2 * c10.X * c12x2 * c12.Y * c21.Y * c13.Y - _
                    6 * c10.Y * c20.X * c13x2 * c21.Y * c13.Y - 6 * c10.Y * c20.Y * c21.X * c13x2 * c13.Y - 2 * c10.Y * c12x2 * c21.X * c12.Y * c13.Y - _
                    2 * c10.Y * c12x2 * c12.Y * c13.X * c21.Y - c11.X * c11.Y * c12x2 * c21.Y * c13.Y - 4 * c11.X * c20.Y * c12.Y * c13x2 * c21.Y - _
                    2 * c11.X * c11y2 * c21.X * c13.X * c13.Y + 3 * c20.X * c11.Y * c12.Y * c13x2 * c21.Y - 2 * c20.X * c12.X * c21.X * c12y2 * c13.Y - _
                    2 * c20.X * c12.X * c12y2 * c13.X * c21.Y - 6 * c20.X * c20.Y * c21.X * c13.X * c13y2 - 2 * c11.Y * c12.X * c20.Y * c13x2 * c21.Y + _
                    3 * c11.Y * c20.Y * c21.X * c12.Y * c13x2 - 2 * c12.X * c20.Y * c21.X * c12y2 * c13.X - c11y2 * c12.X * c21.X * c12.Y * c13.X + _
                    6 * c20.X * c20.Y * c13x2 * c21.Y * c13.Y + 2 * c20.X * c12x2 * c12.Y * c21.Y * c13.Y + 2 * c11x2 * c11.Y * c13.X * c21.Y * c13.Y + _
                    c11x2 * c12.X * c12.Y * c21.Y * c13.Y + 2 * c12x2 * c20.Y * c21.X * c12.Y * c13.Y + 2 * c12x2 * c20.Y * c12.Y * c13.X * c21.Y + _
                    3 * c10x2 * c21.X * c13y3 - 3 * c10y2 * c13x3 * c21.Y + 3 * c20x2 * c21.X * c13y3 + c11y3 * c21.X * c13x2 - c11x3 * c21.Y * c13y2 - _
                    3 * c20y2 * c13x3 * c21.Y - c11.X * c11y2 * c13x2 * c21.Y + c11x2 * c11.Y * c21.X * c13y2 - 3 * c10x2 * c13.X * c21.Y * c13y2 + _
                    3 * c10y2 * c21.X * c13x2 * c13.Y - c11x2 * c12y2 * c13.X * c21.Y + c11y2 * c12x2 * c21.X * c13.Y - 3 * c20x2 * c13.X * c21.Y * c13y2 + _
                    3 * c20y2 * c21.X * c13x2 * c13.Y, _
                c10.X * c10.Y * c11.X * c12.Y * c13.X * c13.Y - c10.X * c10.Y * c11.Y * c12.X * c13.X * c13.Y + c10.X * c11.X * c11.Y * c12.X * c12.Y * c13.Y - _
                    c10.Y * c11.X * c11.Y * c12.X * c12.Y * c13.X - c10.X * c11.X * c20.Y * c12.Y * c13.X * c13.Y + 6 * c10.X * c20.X * c11.Y * c12.Y * c13.X * c13.Y + _
                    c10.X * c11.Y * c12.X * c20.Y * c13.X * c13.Y - c10.Y * c11.X * c20.X * c12.Y * c13.X * c13.Y - 6 * c10.Y * c11.X * c12.X * c20.Y * c13.X * c13.Y + _
                    c10.Y * c20.X * c11.Y * c12.X * c13.X * c13.Y - c11.X * c20.X * c11.Y * c12.X * c12.Y * c13.Y + c11.X * c11.Y * c12.X * c20.Y * c12.Y * c13.X + _
                    c11.X * c20.X * c20.Y * c12.Y * c13.X * c13.Y - c20.X * c11.Y * c12.X * c20.Y * c13.X * c13.Y - 2 * c10.X * c20.X * c12y3 * c13.X + _
                    2 * c10.Y * c12x3 * c20.Y * c13.Y - 3 * c10.X * c10.Y * c11.X * c12.X * c13y2 - 6 * c10.X * c10.Y * c20.X * c13.X * c13y2 + _
                    3 * c10.X * c10.Y * c11.Y * c12.Y * c13x2 - 2 * c10.X * c10.Y * c12.X * c12y2 * c13.X - 2 * c10.X * c11.X * c20.X * c12.Y * c13y2 - _
                    c10.X * c11.X * c11.Y * c12y2 * c13.X + 3 * c10.X * c11.X * c12.X * c20.Y * c13y2 - 4 * c10.X * c20.X * c11.Y * c12.X * c13y2 + _
                    3 * c10.Y * c11.X * c20.X * c12.X * c13y2 + 6 * c10.X * c10.Y * c20.Y * c13x2 * c13.Y + 2 * c10.X * c10.Y * c12x2 * c12.Y * c13.Y + _
                    2 * c10.X * c11.X * c11y2 * c13.X * c13.Y + 2 * c10.X * c20.X * c12.X * c12y2 * c13.Y + 6 * c10.X * c20.X * c20.Y * c13.X * c13y2 - _
                    3 * c10.X * c11.Y * c20.Y * c12.Y * c13x2 + 2 * c10.X * c12.X * c20.Y * c12y2 * c13.X + c10.X * c11y2 * c12.X * c12.Y * c13.X + _
                    c10.Y * c11.X * c11.Y * c12x2 * c13.Y + 4 * c10.Y * c11.X * c20.Y * c12.Y * c13x2 - 3 * c10.Y * c20.X * c11.Y * c12.Y * c13x2 + _
                    2 * c10.Y * c20.X * c12.X * c12y2 * c13.X + 2 * c10.Y * c11.Y * c12.X * c20.Y * c13x2 + c11.X * c20.X * c11.Y * c12y2 * c13.X - _
                    3 * c11.X * c20.X * c12.X * c20.Y * c13y2 - 2 * c10.X * c12x2 * c20.Y * c12.Y * c13.Y - 6 * c10.Y * c20.X * c20.Y * c13x2 * c13.Y - _
                    2 * c10.Y * c20.X * c12x2 * c12.Y * c13.Y - 2 * c10.Y * c11x2 * c11.Y * c13.X * c13.Y - c10.Y * c11x2 * c12.X * c12.Y * c13.Y - _
                    2 * c10.Y * c12x2 * c20.Y * c12.Y * c13.X - 2 * c11.X * c20.X * c11y2 * c13.X * c13.Y - c11.X * c11.Y * c12x2 * c20.Y * c13.Y + _
                    3 * c20.X * c11.Y * c20.Y * c12.Y * c13x2 - 2 * c20.X * c12.X * c20.Y * c12y2 * c13.X - c20.X * c11y2 * c12.X * c12.Y * c13.X + _
                    3 * c10y2 * c11.X * c12.X * c13.X * c13.Y + 3 * c11.X * c12.X * c20y2 * c13.X * c13.Y + 2 * c20.X * c12x2 * c20.Y * c12.Y * c13.Y - _
                    3 * c10x2 * c11.Y * c12.Y * c13.X * c13.Y + 2 * c11x2 * c11.Y * c20.Y * c13.X * c13.Y + c11x2 * c12.X * c20.Y * c12.Y * c13.Y - _
                    3 * c20x2 * c11.Y * c12.Y * c13.X * c13.Y - c10x3 * c13y3 + c10y3 * c13x3 + c20x3 * c13y3 - c20y3 * c13x3 - _
                    3 * c10.X * c20x2 * c13y3 - c10.X * c11y3 * c13x2 + 3 * c10x2 * c20.X * c13y3 + c10.Y * c11x3 * c13y2 + _
                    3 * c10.Y * c20y2 * c13x3 + c20.X * c11y3 * c13x2 + c10x2 * c12y3 * c13.X - 3 * c10y2 * c20.Y * c13x3 - c10y2 * c12x3 * c13.Y + _
                    c20x2 * c12y3 * c13.X - c11x3 * c20.Y * c13y2 - c12x3 * c20y2 * c13.Y - c10.X * c11x2 * c11.Y * c13y2 + _
                    c10.Y * c11.X * c11y2 * c13x2 - 3 * c10.X * c10y2 * c13x2 * c13.Y - c10.X * c11y2 * c12x2 * c13.Y + c10.Y * c11x2 * c12y2 * c13.X - _
                    c11.X * c11y2 * c20.Y * c13x2 + 3 * c10x2 * c10.Y * c13.X * c13y2 + c10x2 * c11.X * c12.Y * c13y2 + _
                    2 * c10x2 * c11.Y * c12.X * c13y2 - 2 * c10y2 * c11.X * c12.Y * c13x2 - c10y2 * c11.Y * c12.X * c13x2 + c11x2 * c20.X * c11.Y * c13y2 - _
                    3 * c10.X * c20y2 * c13x2 * c13.Y + 3 * c10.Y * c20x2 * c13.X * c13y2 + c11.X * c20x2 * c12.Y * c13y2 - 2 * c11.X * c20y2 * c12.Y * c13x2 + _
                    c20.X * c11y2 * c12x2 * c13.Y - c11.Y * c12.X * c20y2 * c13x2 - c10x2 * c12.X * c12y2 * c13.Y - 3 * c10x2 * c20.Y * c13.X * c13y2 + _
                    3 * c10y2 * c20.X * c13x2 * c13.Y + c10y2 * c12x2 * c12.Y * c13.X - c11x2 * c20.Y * c12y2 * c13.X + 2 * c20x2 * c11.Y * c12.X * c13y2 + _
                    3 * c20.X * c20y2 * c13x2 * c13.Y - c20x2 * c12.X * c12y2 * c13.Y - 3 * c20x2 * c20.Y * c13.X * c13y2 + c12x2 * c20y2 * c12.Y * c13.X _
            )
            Dim roots = poly.getRootsInInterval(0, 1)

            For i As Integer = 0 To roots.Count - 1
                Dim s = roots(i)
                Dim xRoots = New Polynomial( _
                    c13.X, _
                    c12.X, _
                    c11.X, _
                        c10.X - c20.X - s * c21.X - s * s * c22.X - s * s * s * c23.X _
                ).getRoots()
                Dim yRoots = New Polynomial( _
                     c13.Y, _
                     c12.Y, _
                     c11.Y, _
                         c10.Y - c20.Y - s * c21.Y - s * s * c22.Y - s * s * s * c23.Y _
                 ).getRoots()

                If (xRoots.Count > 0 And yRoots.Count > 0) Then
                    Dim TOLERANCE = 0.0001


                    For j As Integer = 0 To xRoots.Count - 1
                        Dim xRoot = xRoots(j)

                        If (0 <= xRoot And xRoot <= 1) Then
                            For k As Integer = 0 To yRoots.Count - 1
                                If (Math.Abs(xRoot - yRoots(k)) < TOLERANCE) Then
                                    result.AddPoint( _
                                        c23 * (s * s * s) + (c22 * (s * s) + (c21 * (s) + (c20))))
                                    Exit For
                                End If
                            Next
                        End If
                    Next
                End If
            Next

            If (result.nCount > 0) Then result.status = "Intersection"

            Return result
        End Function



        '/*****
        ''*
        ''*   intersectBezier2Line
        '*
        '*****/
        Public Shared Function intersectBezier2Line(ByVal p1 As Point, ByVal p2 As Point, ByVal p3 As Point, ByVal a1 As Point, ByVal a2 As Point) As Intersection
            Dim a, b As Point           '// temporary variables
            Dim c2, c1, c0 As Point     '// coefficients of quadratic
            Dim cl As Double             '// c coefficient for normal form of line
            Dim n As Point            '// normal for normal form of line
            Dim min = a1.Min(a2) '// used to determine if point is on line segment
            Dim max = a1.Max(a2) '// used to determine if point is on line segment
            Dim result = New Intersection("No Intersection")

            a = p2 * (-2)
            c2 = p1 + (a + p3)

            a = p1 * (-2)
            b = p2 * 2
            c1 = a + b

            c0 = New Point(p1.X, p1.Y)

            '// Convert line to normal form: ax + by + c = 0
            '// Find normal to line: negative inverse of original line's slope
            n = New Point(a1.Y - a2.Y, a2.X - a1.X)

            '// Determine new c coefficient
            cl = a1.X * a2.Y - a2.X * a1.Y

            '// Transform cubic coefficients to line's coordinate system and find roots
            '// of cubic
            Dim roots = New Polynomial(Point.Dot(n, c2), Point.Dot(n, c1), Point.Dot(n, c0) + cl).getRoots()  ' pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp

            '// Any roots in closed interval [0,1] are intersections on Bezier, but
            '// might not be on the line segment.
            '// Find intersections and calculate point coordinates
            For i As Integer = 0 To roots.Count - 1
                Dim t = roots(i)

                If (0 <= t And t <= 1) Then
                    '// We're within the Bezier curve
                    '// Find point on Bezier
                    Dim p4 = Point.lerp(p1, p2, t)
                    Dim p5 = Point.lerp(p2, p3, t)

                    Dim p6 = Point.lerp(p4, p5, t)

                    '// See if point is on line segment
                    '// Had to make special cases for vertical and horizontal lines due
                    '// to slight errors in calculation of p6
                    If (a1.X = a2.X) Then
                        If (min.Y <= p6.Y And p6.Y <= max.Y) Then
                            result.status = "Intersection"
                            result.AddPoint(p6)
                        End If
                    ElseIf (a1.Y = a2.Y) Then
                        If (min.X <= p6.X And p6.X <= max.X) Then
                            result.status = "Intersection"
                            result.AddPoint(p6)
                        End If
                    ElseIf (p6 >= min And p6 <= max) Then
                        result.status = "Intersection"
                        result.AddPoint(p6)
                    End If
                End If
            Next

            Return result
        End Function





        '/*****
        '*
        '*   intersectBezier2Polygon
        '*
        '*****/
        Public Shared Function intersectBezier2Polygon(ByVal p1 As Point, ByVal p2 As Point, ByVal p3 As Point, ByRef points As Point()) As Intersection
            Dim result = New Intersection("No Intersection")
            Dim length As Double = points.Length

            For i As Integer = 0 To length - 1
                Dim a1 = points(i)
                Dim a2 = points(((i + 1) Mod length))
                Dim inter = Intersection.intersectBezier2Line(p1, p2, p3, a1, a2)

                result._points.AddRange(inter._points)
            Next

            If (result.nCount > 0) Then result.status = "Intersection"

            Return result



        End Function



        '/*****
        '*
        '*   intersectBezier2Rectangle
        '*
        '*****/
        Public Shared Function intersectBezier2Rectangle(ByVal p1 As Point, ByVal p2 As Point, _
                                    ByVal p3 As Point, ByVal r1 As Point, ByVal r2 As Point) As Intersection
            Dim min = r1.Min(r2)
            Dim max = r1.Max(r2)
            Dim topRight = New Point(max.X, min.Y)
            Dim bottomLeft = New Point(min.X, max.Y)

            Dim inter1 = Intersection.intersectBezier2Line(p1, p2, p3, min, topRight)
            Dim inter2 = Intersection.intersectBezier2Line(p1, p2, p3, topRight, max)
            Dim inter3 = Intersection.intersectBezier2Line(p1, p2, p3, max, bottomLeft)
            Dim inter4 = Intersection.intersectBezier2Line(p1, p2, p3, bottomLeft, min)

            Dim result = New Intersection("No Intersection")

            result.appendPoints(inter1._points)
            result.appendPoints(inter2._points)
            result.appendPoints(inter3._points)
            result.appendPoints(inter4._points)

            If (result._points.Count > 0) Then result.status = "Intersection"

            Return result
        End Function

        '/*****
        '*
        '*   intersectLinePolygon
        '*
        '*****/
        Public Shared Function intersectLinePolygon(ByVal a1 As Point, ByVal a2 As Point, ByRef points As Point()) As Intersection
            Dim result = New Intersection("No Intersection")
            Dim length = points.Length

            For i As Integer = 0 To length - 1
                Dim b1 = points(i)
                Dim b2 = points((i + 1) Mod length)
                Dim inter = Intersection.intersectLineLine(a1, a2, b1, b2)

                result.appendPoints(inter._points)
            Next

            If (result.nCount > 0) Then result.status = "Intersection"

            Return result
        End Function


        '/*****
        '*
        '*   intersectLineRectangle
        '*
        '*****/
        Public Shared Function intersectLineRectangle(ByVal a1 As Point, ByVal a2 As Point, ByVal r1 As Point, ByVal r2 As Point) As Intersection
            Dim min = r1.Min(r2)
            Dim max = r1.Max(r2)
            Dim topRight = New Point(max.X, min.Y)
            Dim bottomLeft = New Point(min.X, max.Y)

            Dim inter1 = Intersection.intersectLineLine(min, topRight, a1, a2)
            Dim inter2 = Intersection.intersectLineLine(topRight, max, a1, a2)
            Dim inter3 = Intersection.intersectLineLine(max, bottomLeft, a1, a2)
            Dim inter4 = Intersection.intersectLineLine(bottomLeft, min, a1, a2)

            Dim result = New Intersection("No Intersection")

            result.appendPoints(inter1._points)
            result.appendPoints(inter2._points)
            result.appendPoints(inter3._points)
            result.appendPoints(inter4._points)

            If (result.nCount > 0) Then result.status = "Intersection"

            Return result
        End Function

        '/*****
        '*
        '*   intersectPolygonPolygon
        '*
        '*****/
        Public Shared Function intersectPolygonPolygon(ByRef points1 As Point(), ByRef points2 As Point()) As Intersection
            Dim result = New Intersection("No Intersection")
            Dim length = points1.Length

            For i As Integer = 0 To length - 1
                Dim a1 = points1(i)
                Dim a2 = points1((i + 1) Mod length)
                Dim inter = Intersection.intersectLinePolygon(a1, a2, points2)

                result.appendPoints(inter._points)
            Next
            If (result.nCount > 0) Then result.status = "Intersection"

            Return result

        End Function

        Public Shared Function intersectPolygonRectangle(ByRef points As Point(), ByVal r1 As Point, ByVal r2 As Point) As Intersection
            Dim min = r1.Min(r2)
            Dim max = r1.Max(r2)
            Dim topRight = New Point(max.X, min.Y)
            Dim bottomLeft = New Point(min.X, max.Y)

            Dim inter1 = Intersection.intersectLinePolygon(min, topRight, points)
            Dim inter2 = Intersection.intersectLinePolygon(topRight, max, points)
            Dim inter3 = Intersection.intersectLinePolygon(max, bottomLeft, points)
            Dim inter4 = Intersection.intersectLinePolygon(bottomLeft, min, points)

            Dim result = New Intersection("No Intersection")

            result.appendPoints(inter1._points)
            result.appendPoints(inter2._points)
            result.appendPoints(inter3._points)
            result.appendPoints(inter4._points)

            If (result.nCount > 0) Then result.status = "Intersection"

            Return result
        End Function

        '/*****
        '*
        '*   intersectRectangleRectangle
        '*
        '*****/
        Public Shared Function intersectRectangleRectangle(ByVal a1 As Point, ByVal a2 As Point, ByVal b1 As Point, ByVal b2 As Point)
            Dim min = a1.Min(a2)
            Dim max = a1.Max(a2)
            Dim topRight = New Point(max.X, min.Y)
            Dim bottomLeft = New Point(min.X, max.Y)

            Dim inter1 = Intersection.intersectLineRectangle(min, topRight, b1, b2)
            Dim inter2 = Intersection.intersectLineRectangle(topRight, max, b1, b2)
            Dim inter3 = Intersection.intersectLineRectangle(max, bottomLeft, b1, b2)
            Dim inter4 = Intersection.intersectLineRectangle(bottomLeft, min, b1, b2)

            Dim result = New Intersection("No Intersection")

            result.appendPoints(inter1._points)
            result.appendPoints(inter2._points)
            result.appendPoints(inter3._points)
            result.appendPoints(inter4._points)

            If (result._points.Count > 0) Then result.status = "Intersection"

            Return result
        End Function


        '/*****
        '*
        '*   intersectBezier3Line
        '*
        '*   Many thanks to Dan Sunday at SoftSurfer.com.  He gave me a very thorough
        '*   sketch of the algorithm used here.  Without his help, I'm not sure when I
        '*   would have figured out this intersection problem.
        '*
        '*****/
        Public Shared Function intersectBezier3Line(ByVal p1 As Point, ByVal p2 As Point, _
                        ByVal p3 As Point, ByVal p4 As Point, ByVal a1 As Point, ByVal a2 As Point) As Intersection
            Dim a, b, c, d As Point      '// temporary variables
            Dim c3, c2, c1, c0 As Point '// coefficients of cubic
            Dim cl          '// c coefficient for normal form of line
            Dim n As Point           '// normal for normal form of line
            Dim min = a1.Min(a2) '// used to determine if point is on line segment
            Dim max = a1.Max(a2) '// used to determine if point is on line segment
            Dim result = New Intersection("No Intersection")

            '// Start with Bezier using Bernstein polynomials for weighting functions:
            '//     (1-t^3)P1 + 3t(1-t)^2P2 + 3t^2(1-t)P3 + t^3P4
            '//
            '// Expand and collect terms to form linear combinations of original Bezier
            '// controls.  This ends up with a vector cubic in t:
            '//     (-P1+3P2-3P3+P4)t^3 + (3P1-6P2+3P3)t^2 + (-3P1+3P2)t + P1
            '//             /\                  /\                /\       /\
            '//             ||                  ||                ||       ||
            '//             c3                  c2                c1       c0
            '
            '// Calculate the coefficients
            a = p1 * (-1)
            b = p2 * (3)
            c = p3 * (-3)
            d = a + (b + (c + (p4)))
            c3 = New Point(d.X, d.Y)

            a = p1 * (3)
            b = p2 * (-6)
            c = p3 * (3)
            d = a + (b + (c))
            c2 = New Point(d.X, d.Y)

            a = p1 * (-3)
            b = p2 * (3)
            c = a + (b)
            c1 = New Point(c.X, c.Y)

            c0 = New Point(p1.X, p1.Y)

            '// Convert line to normal form: ax + by + c = 0
            '// Find normal to line: negative inverse of original line's slope
            n = New Point(a1.Y - a2.Y, a2.X - a1.X)

            '// Determine new c coefficient
            cl = a1.X * a2.Y - a2.X * a1.Y

            '// ?Rotate each cubic coefficient using line for new coordinate system?
            '// Find roots of rotated cubic
            Dim roots = New Polynomial( _
                 n.Dot(c3), _
                 n.Dot(c2), _
                 n.Dot(c1), _
                 n.Dot(c0) + cl _
             ).getRoots()

            '// Any roots in closed interval [0,1] are intersections on Bezier, but
            '// might not be on the line segment.
            '// Find intersections and calculate point coordinates
            For i As Integer = 0 To roots.Count - 1
                Dim t = roots(i)

                If (0 <= t And t <= 1) Then
                    '// We're within the Bezier curve
                    '// Find point on Bezier
                    Dim p5 = Point.lerp(p1, p2, t)
                    Dim p6 = Point.lerp(p2, p3, t)
                    Dim p7 = Point.lerp(p3, p4, t)

                    Dim p8 = Point.lerp(p5, p6, t)
                    Dim p9 = Point.lerp(p6, p7, t)

                    Dim p10 = Point.lerp(p8, p9, t)

                    '// See if point is on line segment
                    '// Had to make special cases for vertical and horizontal lines due
                    '// to slight errors in calculation of p10
                    If (a1.X = a2.X) Then
                        If (min.Y <= p10.Y And p10.Y <= max.Y) Then
                            result.status = "Intersection"
                            result.AddPoint(p10)
                        End If
                    ElseIf (a1.Y = a2.Y) Then
                        If (min.X <= p10.X And p10.X <= max.X) Then
                            result.status = "Intersection"
                            result.AddPoint(p10)
                        End If
                    ElseIf (p10 >= min And p10 <= max) Then
                        result.status = "Intersection"
                        result.AddPoint(p10)
                    End If
                End If
            Next

            Return result
        End Function

        '/*****
        '*
        '*   intersectBezier3Polygon
        '*
        '*****/
        Public Shared Function intersectBezier3Polygon(ByVal p1 As Point, ByVal p2 As Point, _
                                    ByVal p3 As Point, ByVal p4 As Point, ByVal points As Point()) As Intersection
            Dim result = New Intersection("No Intersection")
            Dim length = points.Length

            For i As Integer = 0 To length - 1
                Dim a1 = points(i)
                Dim a2 = points((i + 1) Mod length)
                Dim inter = Intersection.intersectBezier3Line(p1, p2, p3, p4, a1, a2)

                result.appendPoints(inter._points)
            Next

            If (result._points.Count > 0) Then result.status = "Intersection"

            Return result
        End Function



        '/*****
        '*
        '*   intersectBezier3Rectangle
        '*
        '*****/
        Public Shared Function intersectBezier3Rectangle(ByVal p1 As Point, ByVal p2 As Point, _
                                        ByVal p3 As Point, ByVal p4 As Point, ByVal r1 As Point, ByVal r2 As Point)
            Dim min = r1.Min(r2)
            Dim max = r1.Max(r2)
            Dim topRight = New Point(max.X, min.Y)
            Dim bottomLeft = New Point(min.X, max.Y)

            Dim inter1 = Intersection.intersectBezier3Line(p1, p2, p3, p4, min, topRight)
            Dim inter2 = Intersection.intersectBezier3Line(p1, p2, p3, p4, topRight, max)
            Dim inter3 = Intersection.intersectBezier3Line(p1, p2, p3, p4, max, bottomLeft)
            Dim inter4 = Intersection.intersectBezier3Line(p1, p2, p3, p4, bottomLeft, min)

            Dim result = New Intersection("No Intersection")

            result.appendPoints(inter1._points)
            result.appendPoints(inter2._points)
            result.appendPoints(inter3._points)
            result.appendPoints(inter4._points)

            If (result._points.Count > 0) Then result.status = "Intersection"

            Return result
        End Function
        '/*****
        '*
        '*   intersectBezier2Bezier3
        '*
        '*****/
        Public Shared Function intersectBezier2Bezier3(ByVal a1 As Point, ByVal a2 As Point, _
                        ByVal a3 As Point, ByVal b1 As Point, ByVal b2 As Point, ByVal b3 As Point, ByVal b4 As Point) As Intersection

            Dim a, b, c, d As Point
            Dim c12, c11, c10 As Point
            Dim c23, c22, c21, c20 As Point
            Dim result = New Intersection("No Intersection")

            a = a2 * (-2)
            c12 = a1 + (a + (a3))

            a = a1 * (-2)
            b = a2 * (2)
            c11 = a + (b)

            c10 = New Point(a1.X, a1.Y)

            a = b1 * (-1)
            b = b2 * (3)
            c = b3 * (-3)
            d = a + (b + (c + (b4)))
            c23 = New Point(d.X, d.Y)

            a = b1 * (3)
            b = b2 * (-6)
            c = b3 * (3)
            d = a + (b + (c))
            c22 = New Point(d.X, d.Y)

            a = b1 * (-3)
            b = b2 * (3)
            c = a + (b)
            c21 = New Point(c.X, c.Y)

            c20 = New Point(b1.X, b1.Y)

            Dim c10x2 = c10.X * c10.X
            Dim c10y2 = c10.Y * c10.Y
            Dim c11x2 = c11.X * c11.X
            Dim c11y2 = c11.Y * c11.Y
            Dim c12x2 = c12.X * c12.X
            Dim c12y2 = c12.Y * c12.Y
            Dim c20x2 = c20.X * c20.X
            Dim c20y2 = c20.Y * c20.Y
            Dim c21x2 = c21.X * c21.X
            Dim c21y2 = c21.Y * c21.Y
            Dim c22x2 = c22.X * c22.X
            Dim c22y2 = c22.Y * c22.Y
            Dim c23x2 = c23.X * c23.X
            Dim c23y2 = c23.Y * c23.Y

            Dim poly = New Polynomial( _
        -2 * c12.X * c12.Y * c23.X * c23.Y + c12x2 * c23y2 + c12y2 * c23x2, _
        -2 * c12.X * c12.Y * c22.X * c23.Y - 2 * c12.X * c12.Y * c22.Y * c23.X + 2 * c12y2 * c22.X * c23.X + _
            2 * c12x2 * c22.Y * c23.Y, _
        -2 * c12.X * c21.X * c12.Y * c23.Y - 2 * c12.X * c12.Y * c21.Y * c23.X - 2 * c12.X * c12.Y * c22.X * c22.Y + _
            2 * c21.X * c12y2 * c23.X + c12y2 * c22x2 + c12x2 * (2 * c21.Y * c23.Y + c22y2), _
        2 * c10.X * c12.X * c12.Y * c23.Y + 2 * c10.Y * c12.X * c12.Y * c23.X + c11.X * c11.Y * c12.X * c23.Y + _
            c11.X * c11.Y * c12.Y * c23.X - 2 * c20.X * c12.X * c12.Y * c23.Y - 2 * c12.X * c20.Y * c12.Y * c23.X - _
            2 * c12.X * c21.X * c12.Y * c22.Y - 2 * c12.X * c12.Y * c21.Y * c22.X - 2 * c10.X * c12y2 * c23.X - _
            2 * c10.Y * c12x2 * c23.Y + 2 * c20.X * c12y2 * c23.X + 2 * c21.X * c12y2 * c22.X - _
            c11y2 * c12.X * c23.X - c11x2 * c12.Y * c23.Y + c12x2 * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y), _
        2 * c10.X * c12.X * c12.Y * c22.Y + 2 * c10.Y * c12.X * c12.Y * c22.X + c11.X * c11.Y * c12.X * c22.Y + _
            c11.X * c11.Y * c12.Y * c22.X - 2 * c20.X * c12.X * c12.Y * c22.Y - 2 * c12.X * c20.Y * c12.Y * c22.X - _
            2 * c12.X * c21.X * c12.Y * c21.Y - 2 * c10.X * c12y2 * c22.X - 2 * c10.Y * c12x2 * c22.Y + _
            2 * c20.X * c12y2 * c22.X - c11y2 * c12.X * c22.X - c11x2 * c12.Y * c22.Y + c21x2 * c12y2 + _
            c12x2 * (2 * c20.Y * c22.Y + c21y2), _
        2 * c10.X * c12.X * c12.Y * c21.Y + 2 * c10.Y * c12.X * c21.X * c12.Y + c11.X * c11.Y * c12.X * c21.Y + _
            c11.X * c11.Y * c21.X * c12.Y - 2 * c20.X * c12.X * c12.Y * c21.Y - 2 * c12.X * c20.Y * c21.X * c12.Y - _
            2 * c10.X * c21.X * c12y2 - 2 * c10.Y * c12x2 * c21.Y + 2 * c20.X * c21.X * c12y2 - _
            c11y2 * c12.X * c21.X - c11x2 * c12.Y * c21.Y + 2 * c12x2 * c20.Y * c21.Y, _
        -2 * c10.X * c10.Y * c12.X * c12.Y - c10.X * c11.X * c11.Y * c12.Y - c10.Y * c11.X * c11.Y * c12.X + _
            2 * c10.X * c12.X * c20.Y * c12.Y + 2 * c10.Y * c20.X * c12.X * c12.Y + c11.X * c20.X * c11.Y * c12.Y + _
            c11.X * c11.Y * c12.X * c20.Y - 2 * c20.X * c12.X * c20.Y * c12.Y - 2 * c10.X * c20.X * c12y2 + _
            c10.X * c11y2 * c12.X + c10.Y * c11x2 * c12.Y - 2 * c10.Y * c12x2 * c20.Y - _
            c20.X * c11y2 * c12.X - c11x2 * c20.Y * c12.Y + c10x2 * c12y2 + c10y2 * c12x2 + _
            c20x2 * c12y2 + c12x2 * c20y2 _
    )
            Dim roots = poly.getRootsInInterval(0, 1)

            For i As Integer = 0 To roots.Count - 1
                Dim s = roots(i)
                Dim xRoots = New Polynomial( _
                   c12.X, _
                   c11.X, _
                       c10.X - c20.X - s * c21.X - s * s * c22.X - s * s * s * c23.X _
               ).getRoots()
                Dim yRoots = New Polynomial( _
                   c12.Y, _
                   c11.Y, _
                       c10.Y - c20.Y - s * c21.Y - s * s * c22.Y - s * s * s * c23.Y _
               ).getRoots()

                If (xRoots.Count > 0 And yRoots.Count > 0) Then
                    Dim TOLERANCE = 0.0001


                    For j As Integer = 0 To xRoots.Count - 1
                        Dim xRoot = xRoots(j)

                        If (0 <= xRoot And xRoot <= 1) Then
                            For k As Integer = 0 To yRoots.Count - 1
                                If (Math.Abs(xRoot - yRoots(k)) < TOLERANCE) Then
                                    result._points.Add( _
                                            c23 * (s * s * s) + (c22 * (s * s) + (c21 * (s) + (c20))))

                                    Exit For
                                End If
                            Next
                        End If
                    Next
                End If
            Next

            If (result._points.Count > 0) Then result.status = "Intersection"

            Return result

        End Function

    End Class



End Namespace


