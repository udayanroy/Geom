

Namespace core

    Partial Public Class Intersection

        Public Shared Function intersectBezier3Bezier3(ByVal a1 As Point, ByVal a2 As Point,
                     ByVal a3 As Point, ByVal a4 As Point, ByVal b1 As Point,
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
            Dim poly = New Polynomial(
                -c13x3 * c23y3 + c13y3 * c23x3 - 3 * c13.X * c13y2 * c23x2 * c23.Y +
                    3 * c13x2 * c13.Y * c23.X * c23y2,
                -6 * c13.X * c22.X * c13y2 * c23.X * c23.Y + 6 * c13x2 * c13.Y * c22.Y * c23.X * c23.Y + 3 * c22.X * c13y3 * c23x2 -
                    3 * c13x3 * c22.Y * c23y2 - 3 * c13.X * c13y2 * c22.Y * c23x2 + 3 * c13x2 * c22.X * c13.Y * c23y2,
                -6 * c21.X * c13.X * c13y2 * c23.X * c23.Y - 6 * c13.X * c22.X * c13y2 * c22.Y * c23.X + 6 * c13x2 * c22.X * c13.Y * c22.Y * c23.Y +
                    3 * c21.X * c13y3 * c23x2 + 3 * c22x2 * c13y3 * c23.X + 3 * c21.X * c13x2 * c13.Y * c23y2 - 3 * c13.X * c21.Y * c13y2 * c23x2 -
                    3 * c13.X * c22x2 * c13y2 * c23.Y + c13x2 * c13.Y * c23.X * (6 * c21.Y * c23.Y + 3 * c22y2) + c13x3 * (-c21.Y * c23y2 -
                    2 * c22y2 * c23.Y - c23.Y * (2 * c21.Y * c23.Y + c22y2)),
                c11.X * c12.Y * c13.X * c13.Y * c23.X * c23.Y - c11.Y * c12.X * c13.X * c13.Y * c23.X * c23.Y + 6 * c21.X * c22.X * c13y3 * c23.X +
                    3 * c11.X * c12.X * c13.X * c13.Y * c23y2 + 6 * c10.X * c13.X * c13y2 * c23.X * c23.Y - 3 * c11.X * c12.X * c13y2 * c23.X * c23.Y -
                    3 * c11.Y * c12.Y * c13.X * c13.Y * c23x2 - 6 * c10.Y * c13x2 * c13.Y * c23.X * c23.Y - 6 * c20.X * c13.X * c13y2 * c23.X * c23.Y +
                    3 * c11.Y * c12.Y * c13x2 * c23.X * c23.Y - 2 * c12.X * c12y2 * c13.X * c23.X * c23.Y - 6 * c21.X * c13.X * c22.X * c13y2 * c23.Y -
                    6 * c21.X * c13.X * c13y2 * c22.Y * c23.X - 6 * c13.X * c21.Y * c22.X * c13y2 * c23.X + 6 * c21.X * c13x2 * c13.Y * c22.Y * c23.Y +
                    2 * c12x2 * c12.Y * c13.Y * c23.X * c23.Y + c22x3 * c13y3 - 3 * c10.X * c13y3 * c23x2 + 3 * c10.Y * c13x3 * c23y2 +
                    3 * c20.X * c13y3 * c23x2 + c12y3 * c13.X * c23x2 - c12x3 * c13.Y * c23y2 - 3 * c10.X * c13x2 * c13.Y * c23y2 +
                    3 * c10.Y * c13.X * c13y2 * c23x2 - 2 * c11.X * c12.Y * c13x2 * c23y2 + c11.X * c12.Y * c13y2 * c23x2 - c11.Y * c12.X * c13x2 * c23y2 +
                    2 * c11.Y * c12.X * c13y2 * c23x2 + 3 * c20.X * c13x2 * c13.Y * c23y2 - c12.X * c12y2 * c13.Y * c23x2 -
                    3 * c20.Y * c13.X * c13y2 * c23x2 + c12x2 * c12.Y * c13.X * c23y2 - 3 * c13.X * c22x2 * c13y2 * c22.Y +
                    c13x2 * c13.Y * c23.X * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c13x2 * c22.X * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) +
                    c13x3 * (-2 * c21.Y * c22.Y * c23.Y - c20.Y * c23y2 - c22.Y * (2 * c21.Y * c23.Y + c22y2) - c23.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)),
                6 * c11.X * c12.X * c13.X * c13.Y * c22.Y * c23.Y + c11.X * c12.Y * c13.X * c22.X * c13.Y * c23.Y + c11.X * c12.Y * c13.X * c13.Y * c22.Y * c23.X -
                    c11.Y * c12.X * c13.X * c22.X * c13.Y * c23.Y - c11.Y * c12.X * c13.X * c13.Y * c22.Y * c23.X - 6 * c11.Y * c12.Y * c13.X * c22.X * c13.Y * c23.X -
                    6 * c10.X * c22.X * c13y3 * c23.X + 6 * c20.X * c22.X * c13y3 * c23.X + 6 * c10.Y * c13x3 * c22.Y * c23.Y + 2 * c12y3 * c13.X * c22.X * c23.X -
                    2 * c12x3 * c13.Y * c22.Y * c23.Y + 6 * c10.X * c13.X * c22.X * c13y2 * c23.Y + 6 * c10.X * c13.X * c13y2 * c22.Y * c23.X +
                    6 * c10.Y * c13.X * c22.X * c13y2 * c23.X - 3 * c11.X * c12.X * c22.X * c13y2 * c23.Y - 3 * c11.X * c12.X * c13y2 * c22.Y * c23.X +
                    2 * c11.X * c12.Y * c22.X * c13y2 * c23.X + 4 * c11.Y * c12.X * c22.X * c13y2 * c23.X - 6 * c10.X * c13x2 * c13.Y * c22.Y * c23.Y -
                    6 * c10.Y * c13x2 * c22.X * c13.Y * c23.Y - 6 * c10.Y * c13x2 * c13.Y * c22.Y * c23.X - 4 * c11.X * c12.Y * c13x2 * c22.Y * c23.Y -
                    6 * c20.X * c13.X * c22.X * c13y2 * c23.Y - 6 * c20.X * c13.X * c13y2 * c22.Y * c23.X - 2 * c11.Y * c12.X * c13x2 * c22.Y * c23.Y +
                    3 * c11.Y * c12.Y * c13x2 * c22.X * c23.Y + 3 * c11.Y * c12.Y * c13x2 * c22.Y * c23.X - 2 * c12.X * c12y2 * c13.X * c22.X * c23.Y -
                    2 * c12.X * c12y2 * c13.X * c22.Y * c23.X - 2 * c12.X * c12y2 * c22.X * c13.Y * c23.X - 6 * c20.Y * c13.X * c22.X * c13y2 * c23.X -
                    6 * c21.X * c13.X * c21.Y * c13y2 * c23.X - 6 * c21.X * c13.X * c22.X * c13y2 * c22.Y + 6 * c20.X * c13x2 * c13.Y * c22.Y * c23.Y +
                    2 * c12x2 * c12.Y * c13.X * c22.Y * c23.Y + 2 * c12x2 * c12.Y * c22.X * c13.Y * c23.Y + 2 * c12x2 * c12.Y * c13.Y * c22.Y * c23.X +
                    3 * c21.X * c22x2 * c13y3 + 3 * c21x2 * c13y3 * c23.X - 3 * c13.X * c21.Y * c22x2 * c13y2 - 3 * c21x2 * c13.X * c13y2 * c23.Y +
                    c13x2 * c22.X * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c13x2 * c13.Y * c23.X * (6 * c20.Y * c22.Y + 3 * c21y2) +
                    c21.X * c13x2 * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) + c13x3 * (-2 * c20.Y * c22.Y * c23.Y - c23.Y * (2 * c20.Y * c22.Y + c21y2) -
                    c21.Y * (2 * c21.Y * c23.Y + c22y2) - c22.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)),
                c11.X * c21.X * c12.Y * c13.X * c13.Y * c23.Y + c11.X * c12.Y * c13.X * c21.Y * c13.Y * c23.X + c11.X * c12.Y * c13.X * c22.X * c13.Y * c22.Y -
                    c11.Y * c12.X * c21.X * c13.X * c13.Y * c23.Y - c11.Y * c12.X * c13.X * c21.Y * c13.Y * c23.X - c11.Y * c12.X * c13.X * c22.X * c13.Y * c22.Y -
                    6 * c11.Y * c21.X * c12.Y * c13.X * c13.Y * c23.X - 6 * c10.X * c21.X * c13y3 * c23.X + 6 * c20.X * c21.X * c13y3 * c23.X +
                    2 * c21.X * c12y3 * c13.X * c23.X + 6 * c10.X * c21.X * c13.X * c13y2 * c23.Y + 6 * c10.X * c13.X * c21.Y * c13y2 * c23.X +
                    6 * c10.X * c13.X * c22.X * c13y2 * c22.Y + 6 * c10.Y * c21.X * c13.X * c13y2 * c23.X - 3 * c11.X * c12.X * c21.X * c13y2 * c23.Y -
                    3 * c11.X * c12.X * c21.Y * c13y2 * c23.X - 3 * c11.X * c12.X * c22.X * c13y2 * c22.Y + 2 * c11.X * c21.X * c12.Y * c13y2 * c23.X +
                    4 * c11.Y * c12.X * c21.X * c13y2 * c23.X - 6 * c10.Y * c21.X * c13x2 * c13.Y * c23.Y - 6 * c10.Y * c13x2 * c21.Y * c13.Y * c23.X -
                    6 * c10.Y * c13x2 * c22.X * c13.Y * c22.Y - 6 * c20.X * c21.X * c13.X * c13y2 * c23.Y - 6 * c20.X * c13.X * c21.Y * c13y2 * c23.X -
                    6 * c20.X * c13.X * c22.X * c13y2 * c22.Y + 3 * c11.Y * c21.X * c12.Y * c13x2 * c23.Y - 3 * c11.Y * c12.Y * c13.X * c22x2 * c13.Y +
                    3 * c11.Y * c12.Y * c13x2 * c21.Y * c23.X + 3 * c11.Y * c12.Y * c13x2 * c22.X * c22.Y - 2 * c12.X * c21.X * c12y2 * c13.X * c23.Y -
                    2 * c12.X * c21.X * c12y2 * c13.Y * c23.X - 2 * c12.X * c12y2 * c13.X * c21.Y * c23.X - 2 * c12.X * c12y2 * c13.X * c22.X * c22.Y -
                    6 * c20.Y * c21.X * c13.X * c13y2 * c23.X - 6 * c21.X * c13.X * c21.Y * c22.X * c13y2 + 6 * c20.Y * c13x2 * c21.Y * c13.Y * c23.X +
                    2 * c12x2 * c21.X * c12.Y * c13.Y * c23.Y + 2 * c12x2 * c12.Y * c21.Y * c13.Y * c23.X + 2 * c12x2 * c12.Y * c22.X * c13.Y * c22.Y -
                    3 * c10.X * c22x2 * c13y3 + 3 * c20.X * c22x2 * c13y3 + 3 * c21x2 * c22.X * c13y3 + c12y3 * c13.X * c22x2 +
                    3 * c10.Y * c13.X * c22x2 * c13y2 + c11.X * c12.Y * c22x2 * c13y2 + 2 * c11.Y * c12.X * c22x2 * c13y2 -
                    c12.X * c12y2 * c22x2 * c13.Y - 3 * c20.Y * c13.X * c22x2 * c13y2 - 3 * c21x2 * c13.X * c13y2 * c22.Y +
                    c12x2 * c12.Y * c13.X * (2 * c21.Y * c23.Y + c22y2) + c11.X * c12.X * c13.X * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) +
                    c21.X * c13x2 * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c12x3 * c13.Y * (-2 * c21.Y * c23.Y - c22y2) +
                    c10.Y * c13x3 * (6 * c21.Y * c23.Y + 3 * c22y2) + c11.Y * c12.X * c13x2 * (-2 * c21.Y * c23.Y - c22y2) +
                    c11.X * c12.Y * c13x2 * (-4 * c21.Y * c23.Y - 2 * c22y2) + c10.X * c13x2 * c13.Y * (-6 * c21.Y * c23.Y - 3 * c22y2) +
                    c13x2 * c22.X * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) + c20.X * c13x2 * c13.Y * (6 * c21.Y * c23.Y + 3 * c22y2) +
                    c13x3 * (-2 * c20.Y * c21.Y * c23.Y - c22.Y * (2 * c20.Y * c22.Y + c21y2) - c20.Y * (2 * c21.Y * c23.Y + c22y2) -
                    c21.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)),
                -c10.X * c11.X * c12.Y * c13.X * c13.Y * c23.Y + c10.X * c11.Y * c12.X * c13.X * c13.Y * c23.Y + 6 * c10.X * c11.Y * c12.Y * c13.X * c13.Y * c23.X -
                    6 * c10.Y * c11.X * c12.X * c13.X * c13.Y * c23.Y - c10.Y * c11.X * c12.Y * c13.X * c13.Y * c23.X + c10.Y * c11.Y * c12.X * c13.X * c13.Y * c23.X +
                    c11.X * c11.Y * c12.X * c12.Y * c13.X * c23.Y - c11.X * c11.Y * c12.X * c12.Y * c13.Y * c23.X + c11.X * c20.X * c12.Y * c13.X * c13.Y * c23.Y +
                    c11.X * c20.Y * c12.Y * c13.X * c13.Y * c23.X + c11.X * c21.X * c12.Y * c13.X * c13.Y * c22.Y + c11.X * c12.Y * c13.X * c21.Y * c22.X * c13.Y -
                    c20.X * c11.Y * c12.X * c13.X * c13.Y * c23.Y - 6 * c20.X * c11.Y * c12.Y * c13.X * c13.Y * c23.X - c11.Y * c12.X * c20.Y * c13.X * c13.Y * c23.X -
                    c11.Y * c12.X * c21.X * c13.X * c13.Y * c22.Y - c11.Y * c12.X * c13.X * c21.Y * c22.X * c13.Y - 6 * c11.Y * c21.X * c12.Y * c13.X * c22.X * c13.Y -
                    6 * c10.X * c20.X * c13y3 * c23.X - 6 * c10.X * c21.X * c22.X * c13y3 - 2 * c10.X * c12y3 * c13.X * c23.X + 6 * c20.X * c21.X * c22.X * c13y3 +
                    2 * c20.X * c12y3 * c13.X * c23.X + 2 * c21.X * c12y3 * c13.X * c22.X + 2 * c10.Y * c12x3 * c13.Y * c23.Y - 6 * c10.X * c10.Y * c13.X * c13y2 * c23.X +
                    3 * c10.X * c11.X * c12.X * c13y2 * c23.Y - 2 * c10.X * c11.X * c12.Y * c13y2 * c23.X - 4 * c10.X * c11.Y * c12.X * c13y2 * c23.X +
                    3 * c10.Y * c11.X * c12.X * c13y2 * c23.X + 6 * c10.X * c10.Y * c13x2 * c13.Y * c23.Y + 6 * c10.X * c20.X * c13.X * c13y2 * c23.Y -
                    3 * c10.X * c11.Y * c12.Y * c13x2 * c23.Y + 2 * c10.X * c12.X * c12y2 * c13.X * c23.Y + 2 * c10.X * c12.X * c12y2 * c13.Y * c23.X +
                    6 * c10.X * c20.Y * c13.X * c13y2 * c23.X + 6 * c10.X * c21.X * c13.X * c13y2 * c22.Y + 6 * c10.X * c13.X * c21.Y * c22.X * c13y2 +
                    4 * c10.Y * c11.X * c12.Y * c13x2 * c23.Y + 6 * c10.Y * c20.X * c13.X * c13y2 * c23.X + 2 * c10.Y * c11.Y * c12.X * c13x2 * c23.Y -
                    3 * c10.Y * c11.Y * c12.Y * c13x2 * c23.X + 2 * c10.Y * c12.X * c12y2 * c13.X * c23.X + 6 * c10.Y * c21.X * c13.X * c22.X * c13y2 -
                    3 * c11.X * c20.X * c12.X * c13y2 * c23.Y + 2 * c11.X * c20.X * c12.Y * c13y2 * c23.X + c11.X * c11.Y * c12y2 * c13.X * c23.X -
                    3 * c11.X * c12.X * c20.Y * c13y2 * c23.X - 3 * c11.X * c12.X * c21.X * c13y2 * c22.Y - 3 * c11.X * c12.X * c21.Y * c22.X * c13y2 +
                    2 * c11.X * c21.X * c12.Y * c22.X * c13y2 + 4 * c20.X * c11.Y * c12.X * c13y2 * c23.X + 4 * c11.Y * c12.X * c21.X * c22.X * c13y2 -
                    2 * c10.X * c12x2 * c12.Y * c13.Y * c23.Y - 6 * c10.Y * c20.X * c13x2 * c13.Y * c23.Y - 6 * c10.Y * c20.Y * c13x2 * c13.Y * c23.X -
                    6 * c10.Y * c21.X * c13x2 * c13.Y * c22.Y - 2 * c10.Y * c12x2 * c12.Y * c13.X * c23.Y - 2 * c10.Y * c12x2 * c12.Y * c13.Y * c23.X -
                    6 * c10.Y * c13x2 * c21.Y * c22.X * c13.Y - c11.X * c11.Y * c12x2 * c13.Y * c23.Y - 2 * c11.X * c11y2 * c13.X * c13.Y * c23.X +
                    3 * c20.X * c11.Y * c12.Y * c13x2 * c23.Y - 2 * c20.X * c12.X * c12y2 * c13.X * c23.Y - 2 * c20.X * c12.X * c12y2 * c13.Y * c23.X -
                    6 * c20.X * c20.Y * c13.X * c13y2 * c23.X - 6 * c20.X * c21.X * c13.X * c13y2 * c22.Y - 6 * c20.X * c13.X * c21.Y * c22.X * c13y2 +
                    3 * c11.Y * c20.Y * c12.Y * c13x2 * c23.X + 3 * c11.Y * c21.X * c12.Y * c13x2 * c22.Y + 3 * c11.Y * c12.Y * c13x2 * c21.Y * c22.X -
                    2 * c12.X * c20.Y * c12y2 * c13.X * c23.X - 2 * c12.X * c21.X * c12y2 * c13.X * c22.Y - 2 * c12.X * c21.X * c12y2 * c22.X * c13.Y -
                    2 * c12.X * c12y2 * c13.X * c21.Y * c22.X - 6 * c20.Y * c21.X * c13.X * c22.X * c13y2 - c11y2 * c12.X * c12.Y * c13.X * c23.X +
                    2 * c20.X * c12x2 * c12.Y * c13.Y * c23.Y + 6 * c20.Y * c13x2 * c21.Y * c22.X * c13.Y + 2 * c11x2 * c11.Y * c13.X * c13.Y * c23.Y +
                    c11x2 * c12.X * c12.Y * c13.Y * c23.Y + 2 * c12x2 * c20.Y * c12.Y * c13.Y * c23.X + 2 * c12x2 * c21.X * c12.Y * c13.Y * c22.Y +
                    2 * c12x2 * c12.Y * c21.Y * c22.X * c13.Y + c21x3 * c13y3 + 3 * c10x2 * c13y3 * c23.X - 3 * c10y2 * c13x3 * c23.Y +
                    3 * c20x2 * c13y3 * c23.X + c11y3 * c13x2 * c23.X - c11x3 * c13y2 * c23.Y - c11.X * c11y2 * c13x2 * c23.Y +
                    c11x2 * c11.Y * c13y2 * c23.X - 3 * c10x2 * c13.X * c13y2 * c23.Y + 3 * c10y2 * c13x2 * c13.Y * c23.X - c11x2 * c12y2 * c13.X * c23.Y +
                    c11y2 * c12x2 * c13.Y * c23.X - 3 * c21x2 * c13.X * c21.Y * c13y2 - 3 * c20x2 * c13.X * c13y2 * c23.Y + 3 * c20y2 * c13x2 * c13.Y * c23.X +
                    c11.X * c12.X * c13.X * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c12x3 * c13.Y * (-2 * c20.Y * c23.Y - 2 * c21.Y * c22.Y) +
                    c10.Y * c13x3 * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) + c11.Y * c12.X * c13x2 * (-2 * c20.Y * c23.Y - 2 * c21.Y * c22.Y) +
                    c12x2 * c12.Y * c13.X * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y) + c11.X * c12.Y * c13x2 * (-4 * c20.Y * c23.Y - 4 * c21.Y * c22.Y) +
                    c10.X * c13x2 * c13.Y * (-6 * c20.Y * c23.Y - 6 * c21.Y * c22.Y) + c20.X * c13x2 * c13.Y * (6 * c20.Y * c23.Y + 6 * c21.Y * c22.Y) +
                    c21.X * c13x2 * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) + c13x3 * (-2 * c20.Y * c21.Y * c22.Y - c20y2 * c23.Y -
                    c21.Y * (2 * c20.Y * c22.Y + c21y2) - c20.Y * (2 * c20.Y * c23.Y + 2 * c21.Y * c22.Y)),
                -c10.X * c11.X * c12.Y * c13.X * c13.Y * c22.Y + c10.X * c11.Y * c12.X * c13.X * c13.Y * c22.Y + 6 * c10.X * c11.Y * c12.Y * c13.X * c22.X * c13.Y -
                    6 * c10.Y * c11.X * c12.X * c13.X * c13.Y * c22.Y - c10.Y * c11.X * c12.Y * c13.X * c22.X * c13.Y + c10.Y * c11.Y * c12.X * c13.X * c22.X * c13.Y +
                    c11.X * c11.Y * c12.X * c12.Y * c13.X * c22.Y - c11.X * c11.Y * c12.X * c12.Y * c22.X * c13.Y + c11.X * c20.X * c12.Y * c13.X * c13.Y * c22.Y +
                    c11.X * c20.Y * c12.Y * c13.X * c22.X * c13.Y + c11.X * c21.X * c12.Y * c13.X * c21.Y * c13.Y - c20.X * c11.Y * c12.X * c13.X * c13.Y * c22.Y -
                    6 * c20.X * c11.Y * c12.Y * c13.X * c22.X * c13.Y - c11.Y * c12.X * c20.Y * c13.X * c22.X * c13.Y - c11.Y * c12.X * c21.X * c13.X * c21.Y * c13.Y -
                    6 * c10.X * c20.X * c22.X * c13y3 - 2 * c10.X * c12y3 * c13.X * c22.X + 2 * c20.X * c12y3 * c13.X * c22.X + 2 * c10.Y * c12x3 * c13.Y * c22.Y -
                    6 * c10.X * c10.Y * c13.X * c22.X * c13y2 + 3 * c10.X * c11.X * c12.X * c13y2 * c22.Y - 2 * c10.X * c11.X * c12.Y * c22.X * c13y2 -
                    4 * c10.X * c11.Y * c12.X * c22.X * c13y2 + 3 * c10.Y * c11.X * c12.X * c22.X * c13y2 + 6 * c10.X * c10.Y * c13x2 * c13.Y * c22.Y +
                    6 * c10.X * c20.X * c13.X * c13y2 * c22.Y - 3 * c10.X * c11.Y * c12.Y * c13x2 * c22.Y + 2 * c10.X * c12.X * c12y2 * c13.X * c22.Y +
                    2 * c10.X * c12.X * c12y2 * c22.X * c13.Y + 6 * c10.X * c20.Y * c13.X * c22.X * c13y2 + 6 * c10.X * c21.X * c13.X * c21.Y * c13y2 +
                    4 * c10.Y * c11.X * c12.Y * c13x2 * c22.Y + 6 * c10.Y * c20.X * c13.X * c22.X * c13y2 + 2 * c10.Y * c11.Y * c12.X * c13x2 * c22.Y -
                    3 * c10.Y * c11.Y * c12.Y * c13x2 * c22.X + 2 * c10.Y * c12.X * c12y2 * c13.X * c22.X - 3 * c11.X * c20.X * c12.X * c13y2 * c22.Y +
                    2 * c11.X * c20.X * c12.Y * c22.X * c13y2 + c11.X * c11.Y * c12y2 * c13.X * c22.X - 3 * c11.X * c12.X * c20.Y * c22.X * c13y2 -
                    3 * c11.X * c12.X * c21.X * c21.Y * c13y2 + 4 * c20.X * c11.Y * c12.X * c22.X * c13y2 - 2 * c10.X * c12x2 * c12.Y * c13.Y * c22.Y -
                    6 * c10.Y * c20.X * c13x2 * c13.Y * c22.Y - 6 * c10.Y * c20.Y * c13x2 * c22.X * c13.Y - 6 * c10.Y * c21.X * c13x2 * c21.Y * c13.Y -
                    2 * c10.Y * c12x2 * c12.Y * c13.X * c22.Y - 2 * c10.Y * c12x2 * c12.Y * c22.X * c13.Y - c11.X * c11.Y * c12x2 * c13.Y * c22.Y -
                    2 * c11.X * c11y2 * c13.X * c22.X * c13.Y + 3 * c20.X * c11.Y * c12.Y * c13x2 * c22.Y - 2 * c20.X * c12.X * c12y2 * c13.X * c22.Y -
                    2 * c20.X * c12.X * c12y2 * c22.X * c13.Y - 6 * c20.X * c20.Y * c13.X * c22.X * c13y2 - 6 * c20.X * c21.X * c13.X * c21.Y * c13y2 +
                    3 * c11.Y * c20.Y * c12.Y * c13x2 * c22.X + 3 * c11.Y * c21.X * c12.Y * c13x2 * c21.Y - 2 * c12.X * c20.Y * c12y2 * c13.X * c22.X -
                    2 * c12.X * c21.X * c12y2 * c13.X * c21.Y - c11y2 * c12.X * c12.Y * c13.X * c22.X + 2 * c20.X * c12x2 * c12.Y * c13.Y * c22.Y -
                    3 * c11.Y * c21x2 * c12.Y * c13.X * c13.Y + 6 * c20.Y * c21.X * c13x2 * c21.Y * c13.Y + 2 * c11x2 * c11.Y * c13.X * c13.Y * c22.Y +
                    c11x2 * c12.X * c12.Y * c13.Y * c22.Y + 2 * c12x2 * c20.Y * c12.Y * c22.X * c13.Y + 2 * c12x2 * c21.X * c12.Y * c21.Y * c13.Y -
                    3 * c10.X * c21x2 * c13y3 + 3 * c20.X * c21x2 * c13y3 + 3 * c10x2 * c22.X * c13y3 - 3 * c10y2 * c13x3 * c22.Y + 3 * c20x2 * c22.X * c13y3 +
                    c21x2 * c12y3 * c13.X + c11y3 * c13x2 * c22.X - c11x3 * c13y2 * c22.Y + 3 * c10.Y * c21x2 * c13.X * c13y2 -
                    c11.X * c11y2 * c13x2 * c22.Y + c11.X * c21x2 * c12.Y * c13y2 + 2 * c11.Y * c12.X * c21x2 * c13y2 + c11x2 * c11.Y * c22.X * c13y2 -
                    c12.X * c21x2 * c12y2 * c13.Y - 3 * c20.Y * c21x2 * c13.X * c13y2 - 3 * c10x2 * c13.X * c13y2 * c22.Y + 3 * c10y2 * c13x2 * c22.X * c13.Y -
                    c11x2 * c12y2 * c13.X * c22.Y + c11y2 * c12x2 * c22.X * c13.Y - 3 * c20x2 * c13.X * c13y2 * c22.Y + 3 * c20y2 * c13x2 * c22.X * c13.Y +
                    c12x2 * c12.Y * c13.X * (2 * c20.Y * c22.Y + c21y2) + c11.X * c12.X * c13.X * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) +
                    c12x3 * c13.Y * (-2 * c20.Y * c22.Y - c21y2) + c10.Y * c13x3 * (6 * c20.Y * c22.Y + 3 * c21y2) +
                    c11.Y * c12.X * c13x2 * (-2 * c20.Y * c22.Y - c21y2) + c11.X * c12.Y * c13x2 * (-4 * c20.Y * c22.Y - 2 * c21y2) +
                    c10.X * c13x2 * c13.Y * (-6 * c20.Y * c22.Y - 3 * c21y2) + c20.X * c13x2 * c13.Y * (6 * c20.Y * c22.Y + 3 * c21y2) +
                    c13x3 * (-2 * c20.Y * c21y2 - c20y2 * c22.Y - c20.Y * (2 * c20.Y * c22.Y + c21y2)),
                -c10.X * c11.X * c12.Y * c13.X * c21.Y * c13.Y + c10.X * c11.Y * c12.X * c13.X * c21.Y * c13.Y + 6 * c10.X * c11.Y * c21.X * c12.Y * c13.X * c13.Y -
                    6 * c10.Y * c11.X * c12.X * c13.X * c21.Y * c13.Y - c10.Y * c11.X * c21.X * c12.Y * c13.X * c13.Y + c10.Y * c11.Y * c12.X * c21.X * c13.X * c13.Y -
                    c11.X * c11.Y * c12.X * c21.X * c12.Y * c13.Y + c11.X * c11.Y * c12.X * c12.Y * c13.X * c21.Y + c11.X * c20.X * c12.Y * c13.X * c21.Y * c13.Y +
                    6 * c11.X * c12.X * c20.Y * c13.X * c21.Y * c13.Y + c11.X * c20.Y * c21.X * c12.Y * c13.X * c13.Y - c20.X * c11.Y * c12.X * c13.X * c21.Y * c13.Y -
                    6 * c20.X * c11.Y * c21.X * c12.Y * c13.X * c13.Y - c11.Y * c12.X * c20.Y * c21.X * c13.X * c13.Y - 6 * c10.X * c20.X * c21.X * c13y3 -
                    2 * c10.X * c21.X * c12y3 * c13.X + 6 * c10.Y * c20.Y * c13x3 * c21.Y + 2 * c20.X * c21.X * c12y3 * c13.X + 2 * c10.Y * c12x3 * c21.Y * c13.Y -
                    2 * c12x3 * c20.Y * c21.Y * c13.Y - 6 * c10.X * c10.Y * c21.X * c13.X * c13y2 + 3 * c10.X * c11.X * c12.X * c21.Y * c13y2 -
                    2 * c10.X * c11.X * c21.X * c12.Y * c13y2 - 4 * c10.X * c11.Y * c12.X * c21.X * c13y2 + 3 * c10.Y * c11.X * c12.X * c21.X * c13y2 +
                    6 * c10.X * c10.Y * c13x2 * c21.Y * c13.Y + 6 * c10.X * c20.X * c13.X * c21.Y * c13y2 - 3 * c10.X * c11.Y * c12.Y * c13x2 * c21.Y +
                    2 * c10.X * c12.X * c21.X * c12y2 * c13.Y + 2 * c10.X * c12.X * c12y2 * c13.X * c21.Y + 6 * c10.X * c20.Y * c21.X * c13.X * c13y2 +
                    4 * c10.Y * c11.X * c12.Y * c13x2 * c21.Y + 6 * c10.Y * c20.X * c21.X * c13.X * c13y2 + 2 * c10.Y * c11.Y * c12.X * c13x2 * c21.Y -
                    3 * c10.Y * c11.Y * c21.X * c12.Y * c13x2 + 2 * c10.Y * c12.X * c21.X * c12y2 * c13.X - 3 * c11.X * c20.X * c12.X * c21.Y * c13y2 +
                    2 * c11.X * c20.X * c21.X * c12.Y * c13y2 + c11.X * c11.Y * c21.X * c12y2 * c13.X - 3 * c11.X * c12.X * c20.Y * c21.X * c13y2 +
                    4 * c20.X * c11.Y * c12.X * c21.X * c13y2 - 6 * c10.X * c20.Y * c13x2 * c21.Y * c13.Y - 2 * c10.X * c12x2 * c12.Y * c21.Y * c13.Y -
                    6 * c10.Y * c20.X * c13x2 * c21.Y * c13.Y - 6 * c10.Y * c20.Y * c21.X * c13x2 * c13.Y - 2 * c10.Y * c12x2 * c21.X * c12.Y * c13.Y -
                    2 * c10.Y * c12x2 * c12.Y * c13.X * c21.Y - c11.X * c11.Y * c12x2 * c21.Y * c13.Y - 4 * c11.X * c20.Y * c12.Y * c13x2 * c21.Y -
                    2 * c11.X * c11y2 * c21.X * c13.X * c13.Y + 3 * c20.X * c11.Y * c12.Y * c13x2 * c21.Y - 2 * c20.X * c12.X * c21.X * c12y2 * c13.Y -
                    2 * c20.X * c12.X * c12y2 * c13.X * c21.Y - 6 * c20.X * c20.Y * c21.X * c13.X * c13y2 - 2 * c11.Y * c12.X * c20.Y * c13x2 * c21.Y +
                    3 * c11.Y * c20.Y * c21.X * c12.Y * c13x2 - 2 * c12.X * c20.Y * c21.X * c12y2 * c13.X - c11y2 * c12.X * c21.X * c12.Y * c13.X +
                    6 * c20.X * c20.Y * c13x2 * c21.Y * c13.Y + 2 * c20.X * c12x2 * c12.Y * c21.Y * c13.Y + 2 * c11x2 * c11.Y * c13.X * c21.Y * c13.Y +
                    c11x2 * c12.X * c12.Y * c21.Y * c13.Y + 2 * c12x2 * c20.Y * c21.X * c12.Y * c13.Y + 2 * c12x2 * c20.Y * c12.Y * c13.X * c21.Y +
                    3 * c10x2 * c21.X * c13y3 - 3 * c10y2 * c13x3 * c21.Y + 3 * c20x2 * c21.X * c13y3 + c11y3 * c21.X * c13x2 - c11x3 * c21.Y * c13y2 -
                    3 * c20y2 * c13x3 * c21.Y - c11.X * c11y2 * c13x2 * c21.Y + c11x2 * c11.Y * c21.X * c13y2 - 3 * c10x2 * c13.X * c21.Y * c13y2 +
                    3 * c10y2 * c21.X * c13x2 * c13.Y - c11x2 * c12y2 * c13.X * c21.Y + c11y2 * c12x2 * c21.X * c13.Y - 3 * c20x2 * c13.X * c21.Y * c13y2 +
                    3 * c20y2 * c21.X * c13x2 * c13.Y,
                c10.X * c10.Y * c11.X * c12.Y * c13.X * c13.Y - c10.X * c10.Y * c11.Y * c12.X * c13.X * c13.Y + c10.X * c11.X * c11.Y * c12.X * c12.Y * c13.Y -
                    c10.Y * c11.X * c11.Y * c12.X * c12.Y * c13.X - c10.X * c11.X * c20.Y * c12.Y * c13.X * c13.Y + 6 * c10.X * c20.X * c11.Y * c12.Y * c13.X * c13.Y +
                    c10.X * c11.Y * c12.X * c20.Y * c13.X * c13.Y - c10.Y * c11.X * c20.X * c12.Y * c13.X * c13.Y - 6 * c10.Y * c11.X * c12.X * c20.Y * c13.X * c13.Y +
                    c10.Y * c20.X * c11.Y * c12.X * c13.X * c13.Y - c11.X * c20.X * c11.Y * c12.X * c12.Y * c13.Y + c11.X * c11.Y * c12.X * c20.Y * c12.Y * c13.X +
                    c11.X * c20.X * c20.Y * c12.Y * c13.X * c13.Y - c20.X * c11.Y * c12.X * c20.Y * c13.X * c13.Y - 2 * c10.X * c20.X * c12y3 * c13.X +
                    2 * c10.Y * c12x3 * c20.Y * c13.Y - 3 * c10.X * c10.Y * c11.X * c12.X * c13y2 - 6 * c10.X * c10.Y * c20.X * c13.X * c13y2 +
                    3 * c10.X * c10.Y * c11.Y * c12.Y * c13x2 - 2 * c10.X * c10.Y * c12.X * c12y2 * c13.X - 2 * c10.X * c11.X * c20.X * c12.Y * c13y2 -
                    c10.X * c11.X * c11.Y * c12y2 * c13.X + 3 * c10.X * c11.X * c12.X * c20.Y * c13y2 - 4 * c10.X * c20.X * c11.Y * c12.X * c13y2 +
                    3 * c10.Y * c11.X * c20.X * c12.X * c13y2 + 6 * c10.X * c10.Y * c20.Y * c13x2 * c13.Y + 2 * c10.X * c10.Y * c12x2 * c12.Y * c13.Y +
                    2 * c10.X * c11.X * c11y2 * c13.X * c13.Y + 2 * c10.X * c20.X * c12.X * c12y2 * c13.Y + 6 * c10.X * c20.X * c20.Y * c13.X * c13y2 -
                    3 * c10.X * c11.Y * c20.Y * c12.Y * c13x2 + 2 * c10.X * c12.X * c20.Y * c12y2 * c13.X + c10.X * c11y2 * c12.X * c12.Y * c13.X +
                    c10.Y * c11.X * c11.Y * c12x2 * c13.Y + 4 * c10.Y * c11.X * c20.Y * c12.Y * c13x2 - 3 * c10.Y * c20.X * c11.Y * c12.Y * c13x2 +
                    2 * c10.Y * c20.X * c12.X * c12y2 * c13.X + 2 * c10.Y * c11.Y * c12.X * c20.Y * c13x2 + c11.X * c20.X * c11.Y * c12y2 * c13.X -
                    3 * c11.X * c20.X * c12.X * c20.Y * c13y2 - 2 * c10.X * c12x2 * c20.Y * c12.Y * c13.Y - 6 * c10.Y * c20.X * c20.Y * c13x2 * c13.Y -
                    2 * c10.Y * c20.X * c12x2 * c12.Y * c13.Y - 2 * c10.Y * c11x2 * c11.Y * c13.X * c13.Y - c10.Y * c11x2 * c12.X * c12.Y * c13.Y -
                    2 * c10.Y * c12x2 * c20.Y * c12.Y * c13.X - 2 * c11.X * c20.X * c11y2 * c13.X * c13.Y - c11.X * c11.Y * c12x2 * c20.Y * c13.Y +
                    3 * c20.X * c11.Y * c20.Y * c12.Y * c13x2 - 2 * c20.X * c12.X * c20.Y * c12y2 * c13.X - c20.X * c11y2 * c12.X * c12.Y * c13.X +
                    3 * c10y2 * c11.X * c12.X * c13.X * c13.Y + 3 * c11.X * c12.X * c20y2 * c13.X * c13.Y + 2 * c20.X * c12x2 * c20.Y * c12.Y * c13.Y -
                    3 * c10x2 * c11.Y * c12.Y * c13.X * c13.Y + 2 * c11x2 * c11.Y * c20.Y * c13.X * c13.Y + c11x2 * c12.X * c20.Y * c12.Y * c13.Y -
                    3 * c20x2 * c11.Y * c12.Y * c13.X * c13.Y - c10x3 * c13y3 + c10y3 * c13x3 + c20x3 * c13y3 - c20y3 * c13x3 -
                    3 * c10.X * c20x2 * c13y3 - c10.X * c11y3 * c13x2 + 3 * c10x2 * c20.X * c13y3 + c10.Y * c11x3 * c13y2 +
                    3 * c10.Y * c20y2 * c13x3 + c20.X * c11y3 * c13x2 + c10x2 * c12y3 * c13.X - 3 * c10y2 * c20.Y * c13x3 - c10y2 * c12x3 * c13.Y +
                    c20x2 * c12y3 * c13.X - c11x3 * c20.Y * c13y2 - c12x3 * c20y2 * c13.Y - c10.X * c11x2 * c11.Y * c13y2 +
                    c10.Y * c11.X * c11y2 * c13x2 - 3 * c10.X * c10y2 * c13x2 * c13.Y - c10.X * c11y2 * c12x2 * c13.Y + c10.Y * c11x2 * c12y2 * c13.X -
                    c11.X * c11y2 * c20.Y * c13x2 + 3 * c10x2 * c10.Y * c13.X * c13y2 + c10x2 * c11.X * c12.Y * c13y2 +
                    2 * c10x2 * c11.Y * c12.X * c13y2 - 2 * c10y2 * c11.X * c12.Y * c13x2 - c10y2 * c11.Y * c12.X * c13x2 + c11x2 * c20.X * c11.Y * c13y2 -
                    3 * c10.X * c20y2 * c13x2 * c13.Y + 3 * c10.Y * c20x2 * c13.X * c13y2 + c11.X * c20x2 * c12.Y * c13y2 - 2 * c11.X * c20y2 * c12.Y * c13x2 +
                    c20.X * c11y2 * c12x2 * c13.Y - c11.Y * c12.X * c20y2 * c13x2 - c10x2 * c12.X * c12y2 * c13.Y - 3 * c10x2 * c20.Y * c13.X * c13y2 +
                    3 * c10y2 * c20.X * c13x2 * c13.Y + c10y2 * c12x2 * c12.Y * c13.X - c11x2 * c20.Y * c12y2 * c13.X + 2 * c20x2 * c11.Y * c12.X * c13y2 +
                    3 * c20.X * c20y2 * c13x2 * c13.Y - c20x2 * c12.X * c12y2 * c13.Y - 3 * c20x2 * c20.Y * c13.X * c13y2 + c12x2 * c20y2 * c12.Y * c13.X
            )
            Dim roots = poly.getRootsInInterval(0, 1)

            For i As Integer = 0 To roots.Count - 1
                Dim s = roots(i)
                Dim xRoots = New Polynomial(
                    c13.X,
                    c12.X,
                    c11.X,
                        c10.X - c20.X - s * c21.X - s * s * c22.X - s * s * s * c23.X
                ).getRoots()
                Dim yRoots = New Polynomial(
                     c13.Y,
                     c12.Y,
                     c11.Y,
                         c10.Y - c20.Y - s * c21.Y - s * s * c22.Y - s * s * s * c23.Y
                 ).getRoots()

                If (xRoots.Count > 0 And yRoots.Count > 0) Then
                    Dim TOLERANCE = 0.0001


                    For j As Integer = 0 To xRoots.Count - 1
                        Dim xRoot = xRoots(j)

                        If (0 <= xRoot And xRoot <= 1) Then
                            For k As Integer = 0 To yRoots.Count - 1
                                If (Math.Abs(xRoot - yRoots(k)) < TOLERANCE) Then
                                    result.AddPoint(
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

    End Class



End Namespace


