



Public Class Polynomial
    Private Const TOLERANCE As Double = 0.000001
    Private Const ACCURACY As Integer = 6


    Private coefs As List(Of Double)
    Private _variable As String
    Private _s As Double


    Public Sub New(ByVal i As Integer)

        coefs = New List(Of Double)(i)
    End Sub

    Public Sub New(ByVal ParamArray p() As Double)

        coefs = New List(Of Double)

        For i As Integer = p.Length - 1 To 0 Step -1
            Me.coefs.Add(p(i))
        Next


        Me._variable = "t"
        Me._s = 0

    End Sub

    Public Function eval(ByVal x As Double) As Double
        Dim result As Double = 0

        For i As Integer = Me.coefs.Count - 1 To 0 Step -1
            result = result * x + Me.coefs(i)
        Next

        Return result
    End Function
    Public Function getDegree() As Integer
        Return Me.coefs.Count - 1
    End Function
    '/*****
    '*
    '*   getDerivative
    '*
    '*****/
    Public Function getDerivative() As Polynomial
        Dim derivative = New Polynomial()

        For i As Integer = 1 To Me.coefs.Count - 1
            derivative.coefs.Add(i * Me.coefs(i))
        Next

        Return derivative
    End Function



    Public Function add(ByVal poli As Polynomial) As Polynomial
        Dim result = New Polynomial()
        Dim d1 As Integer = getDegree()
        Dim d2 As Integer = poli.getDegree()
        Dim dmax As Integer = Math.Max(d1, d2)

        For i As Integer = 0 To dmax
            Dim v1 As Double = IIf((i <= d1), coefs(i), 0)
            Dim v2 As Double = IIf((i <= d2), poli.coefs(i), 0)
            result.coefs(i) = v1 + v2
        Next
        Return result
    End Function


    '*****
    '*
    '*   multiply
    '*
    '*****/
    Public Function multiply(ByVal poli As Polynomial) As Polynomial
        Dim result = New Polynomial()

        For i As Integer = 0 To Me.getDegree() + poli.getDegree()
            result.coefs.Add(0)
        Next

        For i As Integer = 0 To Me.getDegree()
            For j As Integer = 0 To poli.getDegree()
                result.coefs(i + j) += Me.coefs(i) * poli.coefs(j)
            Next
        Next

        Return result
    End Function

    ' /*****
    '*
    '*   divide_scalar
    '*
    '*****/
    Public Sub divide_scalar(ByVal scalar As Double)
        For i As Integer = 0 To Me.coefs.Count - 1
            Me.coefs(i) /= scalar
        Next
    End Sub

    '/*****
    '*
    '*   simplify
    '*
    '*****/
    Public Sub simplify()
        For i As Integer = Me.getDegree() To 0 Step -1
            If (Math.Abs(Me.coefs(i)) <= Polynomial.TOLERANCE) Then
                Me.coefs.RemoveAt(Me.coefs.Count - 1)
            Else
                Exit For
            End If
        Next
    End Sub


    '/*****
    '*
    '*   bisection
    '*
    '*****/
    Public Function bisection(ByVal min As Double, ByVal max As Double) As Double
        Dim minValue = Me.eval(min)
        Dim maxValue = Me.eval(max)

        Dim result As Double = Double.NaN

        If (Math.Abs(minValue) <= Polynomial.TOLERANCE) Then
            result = min
        ElseIf (Math.Abs(maxValue) <= Polynomial.TOLERANCE) Then
            result = max

        ElseIf (minValue * maxValue <= 0) Then



            Dim tmp1 = Math.Log(max - min)
            Dim tmp2 = Math.Log(10) * Polynomial.ACCURACY
            Dim iters = Math.Round((tmp1 + tmp2) / Math.Log(2))
            'Debug.Print("iter- " & iters)

            If Double.IsNaN(tmp1) = False Then
                For i As Integer = 0 To iters - 1
                    result = 0.5 * (min + max)
                    Dim value = Me.eval(result)
                    If (Math.Abs(value) <= Polynomial.TOLERANCE) Then Exit For

                    If (value * minValue < 0) Then
                        ' Debug.Print("True")
                        max = result
                        maxValue = value
                    Else
                        'Debug.Print("False")
                        min = result
                        minValue = value
                    End If
                    'Debug.Print("min:" & min & " max:" & max)
                    ' Debug.Print("minvalue:" & minValue & " max value:" & maxValue)
                    'Debug.Print("result iter- " & result)

                Next
            End If


        End If
        Return result
    End Function


    '/*****
    '*
    '*   toString
    '*
    '*****/
    Public Overrides Function toString() As String

        Dim coefs = New List(Of Double)
        Dim signs = New List(Of String)

        For i As Integer = Me.coefs.Count - 1 To 0 Step -1
            Dim value = Math.Round(Me.coefs(i) * 1000) / 1000
            If (value <> 0) Then
                Dim sign As String = IIf((value < 0), " - ", " + ")

                value = Math.Abs(value)
                If (i > 0) Then
                    If (value = 1) Then
                        value = Me._variable
                    Else
                        value += Me._variable
                    End If

                End If

                If (i > 1) Then value += "^" + i

                signs.Add(sign)
                coefs.Add(value)
            End If

        Next

        signs(0) = IIf((signs(0) = " + "), "", "-")

        Dim result As String = ""
        For i As Integer = 0 To coefs.Count - 1
            result += signs(i) + coefs(i)
        Next


        Return result
    End Function


    '/*****
    '*
    '*   trapezoid
    '*   Based on trapzd in "Numerical Recipes in C", page 137
    '*
    '*****/
    Public Function trapezoid(ByVal min As Integer, ByVal max As Integer, ByVal n As Integer) As Double


        Dim range = max - min
        Dim TOLERANCE = 0.0000001
        If (n = 1) Then
            Dim minValue = Me.eval(min)
            Dim maxValue = Me.eval(max)
            Me._s = 0.5 * range * (minValue + maxValue)

        Else
            Dim it = 1 << (n - 2)
            Dim delta = range / it
            Dim x = min + 0.5 * delta
            Dim sum = 0

            For i As Integer = 0 To it - 1
                sum += Me.eval(x)
                x += delta
                Me._s = 0.5 * (Me._s + range * sum / it)
            Next

        End If
        Return Me._s
    End Function


    '/*****
    '*
    '*   simpson
    '*   Based on trapzd in "Numerical Recipes in C", page 139
    '*
    '*****/
    Public Function simpson(ByVal min As Integer, ByVal max As Integer) As Double
        Dim range = max - min
        Dim st = 0.5 * range * (Me.eval(min) + Me.eval(max))
        Dim t = st
        Dim s = 4.0 * st / 3.0
        Dim os = s
        Dim ost = st
        Dim TOLERANCE = 0.0000001

        Dim it = 1

        For n As Integer = 2 To 20
            Dim delta = range / it
            Dim x = min + 0.5 * delta
            Dim sum = 0

            For i = 1 To it
                sum += Me.eval(x)
                x += delta
            Next
            t = 0.5 * (t + range * sum / it)
            st = t
            s = (4.0 * st - ost) / 3.0

            If (Math.Abs(s - os) < TOLERANCE * Math.Abs(os)) Then Exit For


            os = s
            ost = st
            it <<= 1



        Next

        Return s
    End Function


    '/*****
    '*
    '*   interpolate - class method
    '*
    '*****/

    'Functioncan can produce error result 

    Public Function interpolate(ByVal xs As Double(), ByVal ys As Double(), ByVal n As Integer, ByVal offset As Integer, ByVal x As Integer) As Double()
        Dim y As Double = 0
        Dim dy As Double = 0
        Dim c(n) As Double
        Dim d(n) As Double
        Dim ns = 0
        Dim result(2) As Double

        Dim diff = Math.Abs(x - xs(offset))
        For i As Integer = 0 To n - 1
            Dim dift = Math.Abs(x - xs(offset + i))

            If (dift < diff) Then
                ns = i
                diff = dift
            End If


            d(i) = ys(offset + i)
            c(i) = d(i)
        Next
        y = ys(offset + ns)
        ns -= 1

        For m As Integer = 1 To n - 1
            For i As Integer = 0 To n - m - 1
                Dim ho = xs(offset + i) - x
                Dim hp = xs(offset + i + m) - x
                Dim w = c(i + 1) - d(i)
                Dim den = ho - hp

                If den = 0.0 Then
                    result(0) = 0
                    result(1) = 0
                    Exit For
                End If
                den = w / den
                d(i) = hp * den
                c(i) = ho * den
            Next

            dy = IIf((2 * (ns + 1) < (n - m)), c(ns + 1), d(ns - 1)) ''it can be error
            y += dy

        Next


        result(0) = y
        result(1) = dy

        Return result
    End Function











    '/*****
    '*
    '*   romberg
    '*
    '*****/
    Public Function romberg(ByVal min As Integer, ByVal max As Integer)

        Dim AMAX = 20
        Dim K = 3
        Dim TOLERANCE = 0.000001
        Dim s(AMAX + 1) As Double
        Dim h(AMAX + 1) As Double
        Dim result() As Double = {0, 0}

        h(0) = 1.0
        For j As Integer = 1 To max

            s(j - 1) = Me.trapezoid(min, max, j)
            If (j >= K) Then
                result = Me.interpolate(h, s, K, j - K, 0.0)
                If (Math.Abs(result(1)) <= TOLERANCE * result(0)) Then Exit For
            End If


            s(j) = s(j - 1)
            h(j) = 0.25 * h(j - 1)

        Next
        Return result(0)
    End Function

    '/*****
    '*
    '*   getLinearRoot
    '*
    '*****/
    Public Function getLinearRoot() As List(Of Double)
        Dim result As New List(Of Double)
        Dim a = Me.coefs(1)

        If (a <> 0) Then result.Add(-Me.coefs(0) / a)


        Return result
    End Function


    '/*****
    '*
    '*   getQuadraticRoots
    '*
    '*****/
    Public Function getQuadraticRoots() As List(Of Double)
        Dim results As New List(Of Double)

        If (Me.getDegree() = 2) Then
            Dim a = Me.coefs(2)
            Dim b = Me.coefs(1) / a
            Dim c = Me.coefs(0) / a
            Dim d = b * b - 4 * c

            If (d > 0) Then
                Dim e = Math.Sqrt(d)

                results.Add(0.5 * (-b + e))
                results.Add(0.5 * (-b - e))

            ElseIf (d = 0) Then
                '// really two roots with same value, but we only return one
                results.Add(0.5 * -b)
            End If

        End If



        Return results
    End Function



    '/*****
    '*
    '*   getCubicRoots
    '*
    '*   This code is based on MgcPolynomial.cpp written by David Eberly.  His
    '*   code along with many other excellent examples are avaiable at his site:
    '*   http://www.magic-software.com
    '*
    '*****/
    Public Function getCubicRoots() As List(Of Double)
        Dim results As New List(Of Double)

        If (Me.getDegree() = 3) Then
            Dim c3 = Me.coefs(3)
            Dim c2 = Me.coefs(2) / c3
            Dim c1 = Me.coefs(1) / c3
            Dim c0 = Me.coefs(0) / c3

            Dim a = (3 * c1 - c2 * c2) / 3
            Dim b = (2 * c2 * c2 * c2 - 9 * c1 * c2 + 27 * c0) / 27
            Dim offset = c2 / 3
            Dim discrim = b * b / 4 + a * a * a / 27
            Dim halfB = b / 2

            ' If (Math.Abs(discrim) <= Polynomial.TOLERANCE) Then discrim = 0

            If (discrim > 0) Then
                Dim e = Math.Sqrt(discrim)
                Dim tmp
                Dim root

                tmp = -halfB + e

                If (tmp >= 0) Then
                    root = Math.Pow(tmp, 1 / 3)
                Else
                    root = -Math.Pow(-tmp, 1 / 3)
                End If

                tmp = -halfB - e
                If (tmp >= 0) Then
                    root += Math.Pow(tmp, 1 / 3)
                Else
                    root -= Math.Pow(-tmp, 1 / 3)
                End If
                results.Add(root - offset)

            ElseIf (discrim < 0) Then
                Dim distance = Math.Sqrt(-a / 3)
                Dim angle = Math.Atan2(Math.Sqrt(-discrim), -halfB) / 3
                Dim cos = Math.Cos(angle)
                Dim sin = Math.Sin(angle)
                Dim sqrt3 = Math.Sqrt(3)

                results.Add(2 * distance * cos - offset)
                results.Add(-distance * (cos + sqrt3 * sin) - offset)
                results.Add(-distance * (cos - sqrt3 * sin) - offset)

            Else
                Dim tmp

                If (halfB >= 0) Then
                    tmp = -Math.Pow(halfB, 1 / 3)
                Else
                    tmp = Math.Pow(-halfB, 1 / 3)
                End If

                results.Add(2 * tmp - offset)
                '// really should return next root twice, but we return only one
                results.Add(-tmp - offset)
            End If
        End If

        Return results

    End Function


    '/*****
    '*
    '*   getQuarticRoots
    '*
    '*   This code is based on MgcPolynomial.cpp written by David Eberly.  His
    '*   code along with many other excellent examples are avaiable at his site:
    '*   http://www.magic-software.com
    '*
    '*****/
    Public Function getQuarticRoots() As List(Of Double)
        Dim results As New List(Of Double)

        If (Me.getDegree() = 4) Then
            Dim c4 = Me.coefs(4)
            Dim c3 = Me.coefs(3) / c4
            Dim c2 = Me.coefs(2) / c4
            Dim c1 = Me.coefs(1) / c4
            Dim c0 = Me.coefs(0) / c4

            Dim resolveRoots = New Polynomial(1, -c2, c3 * c1 - 4 * c0, -c3 * c3 * c0 + 4 * c2 * c0 - c1 * c1).getCubicRoots()
            Dim y = resolveRoots(0)
            Dim discrim = c3 * c3 / 4 - c2 + y

            If (Math.Abs(discrim) <= Polynomial.TOLERANCE) Then discrim = 0

            If (discrim > 0) Then
                Dim e = Math.Sqrt(discrim)
                Dim t1 = 3 * c3 * c3 / 4 - e * e - 2 * c2
                Dim t2 = (4 * c3 * c2 - 8 * c1 - c3 * c3 * c3) / (4 * e)
                Dim plus = t1 + t2
                Dim minus = t1 - t2

                If (Math.Abs(plus) <= Polynomial.TOLERANCE) Then plus = 0
                If (Math.Abs(minus) <= Polynomial.TOLERANCE) Then minus = 0

                If (plus >= 0) Then
                    Dim f = Math.Sqrt(plus)

                    results.Add(-c3 / 4 + (e + f) / 2)
                    results.Add(-c3 / 4 + (e - f) / 2)
                End If
                If (minus >= 0) Then
                    Dim f = Math.Sqrt(minus)

                    results.Add(-c3 / 4 + (f - e) / 2)
                    results.Add(-c3 / 4 - (f + e) / 2)

                End If


            ElseIf (discrim < 0) Then
                '// no roots

            Else
                Dim t2 = y * y - 4 * c0

                If (t2 >= -Polynomial.TOLERANCE) Then
                    If (t2 < 0) Then t2 = 0

                    t2 = 2 * Math.Sqrt(t2)
                    Dim t1 = 3 * c3 * c3 / 4 - 2 * c2
                    If (t1 + t2 >= Polynomial.TOLERANCE) Then
                        Dim d = Math.Sqrt(t1 + t2)

                        results.Add(-c3 / 4 + d / 2)
                        results.Add(-c3 / 4 - d / 2)
                    End If
                    If (t1 - t2 >= Polynomial.TOLERANCE) Then
                        Dim d = Math.Sqrt(t1 - t2)

                        results.Add(-c3 / 4 + d / 2)
                        results.Add(-c3 / 4 - d / 2)


                    End If
                End If
            End If
        End If
        Return results
    End Function



    '/*****
    '*
    '*'   getRootsInInterval
    '*
    '*****/
    Function getRootsInInterval(ByVal min As Double, ByVal max As Double) As List(Of Double)
        Dim roots As New List(Of Double)
        Dim root As Double

        If (Me.getDegree() = 1) Then
            root = Me.bisection(min, max)
            If Not (Double.IsNaN(root)) Then roots.Add(root)

        Else
            ' // get roots of derivative
            Dim deriv = Me.getDerivative()
            Dim droots As New List(Of Double)
            droots = deriv.getRootsInInterval(min, max)
            If (droots.Count > 0) Then
                '// find root on [min, droots[0]]
                root = Me.bisection(min, droots(0))
                If Not (Double.IsNaN(root)) Then roots.Add(root)
                '// find root on [droots[i],droots[i+1]] for 0 <= i <= count-2
                For i As Integer = 0 To droots.Count - 2
                    root = Me.bisection(droots(i), droots(i + 1))
                    If Not (Double.IsNaN(root)) Then roots.Add(root)
                Next

                '// find root on [droots[count-1],xmax]
                root = Me.bisection(droots(droots.Count - 1), max)
                If Not (Double.IsNaN(root)) Then roots.Add(root)

            Else
                '// polynomial is monotone on [min,max], has at most one root
                root = Me.bisection(min, max)
                If Not (Double.IsNaN(root)) Then roots.Add(root)




            End If
        End If


        Return roots
    End Function


    '/*****
    '*
    '*   getRoots
    '*
    '*****/
    Public Function getRoots() As List(Of Double)
        Dim result As New List(Of Double)

        Me.simplify()
        Select Case (Me.getDegree())
            Case 0 : result = New List(Of Double)
            Case 1 : result = Me.getLinearRoot()
            Case 2 : result = Me.getQuadraticRoots()
            Case 3 : result = Me.getCubicRoots()
            Case 4 : result = Me.getQuarticRoots()
            Case Else
                result = New List(Of Double)
                '// should try Newton's method and/or bisection
        End Select

        Return result
    End Function



End Class


