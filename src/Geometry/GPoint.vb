

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

    Public Structure GPoint

        Private x1, y1 As Double



        Public Sub New(ByVal x As Double, ByVal y As Double)
            x1 = x
            y1 = y
        End Sub

        Public Sub New(ByVal x As Integer, ByVal y As Integer)
            x1 = x
            y1 = y
        End Sub



        Public Property X() As Double
            Get
                Return x1
            End Get
            Set(ByVal value As Double)
                x1 = value
            End Set
        End Property
        Public Property Y() As Double
            Get
                Return y1
            End Get
            Set(ByVal value As Double)
                y1 = value
            End Set
        End Property


        Public Sub SetValue(ByVal x As Double, ByVal y As Double)
            Me.x1 = x
            Me.y1 = y

        End Sub
        Public Sub SetValue(ByVal p As GPoint)
            Me.x1 = p.X
            Me.y1 = p.Y

        End Sub


        Public Overrides Function toString() As String
            Return "(" & Me.X & "," & Me.Y & ")"
        End Function

#Region "Operator Overloading "
        Public Function Dot(ByVal p2 As GPoint) As Double
            Return X * p2.X + Y * p2.Y
        End Function





        Public Shared Operator +(ByVal p1 As GPoint, ByVal p2 As GPoint) As GPoint
            Return New GPoint(p1.X + p2.X, p1.Y + p2.Y)
        End Operator
        Public Shared Operator -(ByVal p1 As GPoint, ByVal p2 As GPoint) As GPoint
            Return New GPoint(p1.X - p2.X, p1.Y - p2.Y)
        End Operator
        Public Shared Operator *(ByVal p1 As GPoint, ByVal p2 As GPoint) As GPoint
            Return New GPoint(p1.X * p2.X, p1.Y * p2.Y)
        End Operator

        Public Shared Operator /(ByVal p1 As GPoint, ByVal p2 As GPoint) As GPoint
            Return New GPoint(p1.X / p2.X, p1.Y / p2.Y)
        End Operator

        Public Shared Operator >=(ByVal p1 As GPoint, ByVal p2 As GPoint) As Boolean
            Return (p1.X >= p2.X And p1.Y >= p2.Y)
        End Operator
        Public Shared Operator <=(ByVal p1 As GPoint, ByVal p2 As GPoint) As Boolean
            Return (p1.X <= p2.X And p1.Y <= p2.Y)
        End Operator

        Public Shared Operator +(ByVal p1 As GPoint, ByVal n As Double) As GPoint
            Return New GPoint(p1.X + n, p1.Y + n)
        End Operator
        Public Shared Operator -(ByVal p1 As GPoint, ByVal n As Double) As GPoint
            Return New GPoint(p1.X - n, p1.Y - n)
        End Operator
        Public Shared Operator *(ByVal n As Double, ByVal p1 As GPoint) As GPoint
            Return New GPoint(p1.X * n, p1.Y * n)
        End Operator
        Public Shared Operator *(ByVal p1 As GPoint, ByVal n As Double) As GPoint
            Return New GPoint(p1.X * n, p1.Y * n)
        End Operator
        Public Shared Operator /(ByVal p1 As GPoint, ByVal n As Double) As GPoint
            Return New GPoint(p1.X / n, p1.Y / n)
        End Operator

        Public Shared Operator /(ByVal n As Double, ByVal p1 As GPoint) As GPoint
            Return New GPoint(n / p1.X, n / p1.Y)
        End Operator

        Public Shared Operator -(ByVal p1 As GPoint) As GPoint
            Return New GPoint(-p1.X, -p1.Y)
        End Operator





        Public Shared Function lerp(ByVal p1 As GPoint, ByVal p2 As GPoint, ByVal t As Double) As GPoint
            Return New GPoint(p1.X + (p2.X - p1.X) * t, p1.Y + (p2.Y - p1.Y) * t)
        End Function
        Public Shared Function Dot(ByVal p1 As GPoint, ByVal p2 As GPoint) As Double
            Return p1.X * p2.X + p1.Y * p2.Y
        End Function
        Public Shared Function Cross(ByVal p1 As GPoint, ByVal p2 As GPoint) As Double
            Return Dot(p1, CW(p2))
        End Function
        Public Shared Function CW(ByVal P As GPoint) As GPoint
            Return New GPoint(-P.Y, P.X)
        End Function

        Public Function Min(ByVal pt As GPoint) As GPoint
            Return New GPoint(Math.Min(Me.X, pt.X), Math.Min(Me.Y, pt.Y))
        End Function
        Public Function Max(ByVal pt As GPoint) As GPoint
            Return New GPoint(Math.Max(Me.X, pt.X), Math.Max(Me.Y, pt.Y))
        End Function


#End Region

        Public Shared Function Distance(ByVal p1 As GPoint, ByVal p2 As GPoint) As Double
            Return Math.Sqrt((p2.X - p1.X) ^ 2 + (p2.Y - p1.Y) ^ 2)
        End Function

        Function modulus() As Integer
            Return Me.X + Me.Y
        End Function

    End Structure


End Namespace

