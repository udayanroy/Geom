

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



    Public Class GMatrix

        Const deg2rad As Double = 0.017453292519943295

        Dim _dx, _dy, _m11, _m12, _m21, _m22 As Single


        Public Sub New()

        End Sub

        Public Function isInvertible() As Boolean
            Return ((_m11 * _m22 - _m12 * _m21) < 0.00001)
        End Function

        Public Function determinant() As Single
            Return _m11 * _m22 - _m12 * _m21
        End Function

        Public Function Invert() As Boolean
            Dim dtr = determinant()
            If dtr = 0.0 Then
                Return False
            Else
                Dim dinv = 1.0 / dtr

                Dim mat(6) As Single
                mat(0) = _m22 * dinv
                mat(1) = -_m12 * dinv
                mat(2) = -_m21 * dinv
                mat(3) = _m11 * dinv
                mat(4) = (_m21 * _dy - _m22 * _dx) * dinv
                mat(5) = (_m12 * _dx - _m11 * _dy) * dinv

                _m11 = mat(0)
                _m12 = mat(1)
                _m21 = mat(2)
                _m22 = mat(3)
                _dx = mat(4)
                _dy = mat(5)

                Return True
            End If
        End Function
        Public Sub reset()
            _dx = 0.0F
            _dy = 0.0F
            _m12 = 0.0F
            _m21 = 0.0F

            _m11 = 1.0F
            _m22 = 1.0F
        End Sub

        Public Sub Translate(ByVal dx As Single, ByVal dy As Single)
            _dx += dx * _m11 + dy * _m21
            _dy += dy * _m22 + dx * _m12
        End Sub

        Public Sub Scale(ByVal sx As Single, ByVal sy As Single)
            _m11 *= sx
            _m12 *= sx
            _m21 *= sy
            _m22 *= sy
        End Sub
        Public Sub Roatate(ByVal a As Single)
            Dim sina As Single = 0
            Dim cosa As Single = 0
            If (a = 90 Or a = -270) Then
                sina = 1
            ElseIf (a = 270 Or a = -90) Then
                sina = -1
            ElseIf (a = 180) Then
                cosa = -1
            Else
                Dim b = deg2rad * a
                sina = Math.Sin(b)
                cosa = Math.Cos(b)
            End If
            Dim tm11 = cosa * _m11 + sina * _m21
            Dim tm12 = cosa * _m12 + sina * _m22
            Dim tm21 = -sina * _m11 + cosa * _m21
            Dim tm22 = -sina * _m12 + cosa * _m22
            _m11 = tm11
            _m12 = tm12
            _m21 = tm21
            _m22 = tm22
        End Sub
        Public Sub Shear(ByVal sh As Single, ByVal sv As Single)
            Dim tm11 = sv * _m21
            Dim tm12 = sv * _m22
            Dim tm21 = sh * _m11
            Dim tm22 = sh * _m12
            _m11 += tm11
            _m12 += tm12
            _m21 += tm21
            _m22 += tm22
        End Sub

        Public Sub Transform(ByRef p As GPoint)
            Dim fx = _m11 * p.X + _m21 * p.Y + _dx
            Dim fy = _m12 * p.X + _m22 * p.Y + _dy
            p.X = fx
            p.Y = fy
        End Sub
        Public Sub Transform(ByRef points As GPoint())
            For Each p As GPoint In points
                Me.Transform(p)
            Next
        End Sub


    End Class
End Namespace