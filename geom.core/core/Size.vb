
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


Namespace core


    Public Structure Size

        Public Sub New(ByVal width As Double, ByVal height As Double)
            _Width = width
            _Height = height
        End Sub
        Public Sub New(ByVal p As Point)
            _Width = p.X
            _Height = p.Y
        End Sub
        Public Sub New(ByVal p1 As Point, ByVal p2 As Point)
            _Width = p2.X - p1.X
            _Height = p2.Y - p1.Y
        End Sub

        Public Property Width As Double
        Public Property Height As Double

    End Structure
End Namespace