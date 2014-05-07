------------------------------------------------
-- GLOBAL VARIABLES TO PLAY WITH
------------------------------------------------

-- number of iterations of the Julia Set
maxIterations : Int 
maxIterations = 100 


-- The constant c used in the julia function 
constant : Complex
constant = Complex -0.835 -0.2321

-- The Julia Function
function : Complex -> Complex -> Complex
function c z = (z `cMul` z) `cAdd` c

-- width of the drawing area
canvasWidth : Int
canvasWidth  = 400

-- height of the drawing area
canvasHeight : Int
canvasHeight = 400


-- number of points sampled along the x-axis
xDensity : Int
xDensity = 400

-- number of points sampled along the y-axis
yDensity : Int
yDensity = 400


-- The size of the point (only affects drawing)
pointSize : Float
pointSize = 1

------------------------------------------------
-- COMPLEX NUMBER TYPE
------------------------------------------------

-- A complex number is a number with a real part
-- and an imaginary part
type Complex = { real      : Float
               , imaginary : Float }
               
-- Complex addition
cAdd : Complex -> Complex -> Complex
cAdd z w = Complex (z.real + w.real)
                   (z.imaginary + w.imaginary)
   
-- Complex subtraction
cSub : Complex -> Complex -> Complex
cSub z w = Complex (z.real - w.real)
                   (z.imaginary - w.imaginary)
 
-- Complex multiplication
cMul : Complex -> Complex -> Complex
cMul z w = 
  let a = z.real
      b = z.imaginary
      c = w.real
      d = w.imaginary
  in Complex (a * c - b * d)
             (b * c + a * d)

-- Absolute value of a complex number
cAbs : Complex -> Float
cAbs z = 
  let a = z.real
      b = z.imaginary
  in sqrt (a * a + b * b)
  
  
-- Function to turn a Float into a complex number
-- with real part set to the float and with 
-- 0 imaginary part
toComplex : Float -> Complex
toComplex x   = Complex x 0

-- Function to turn a Float into a complex number
-- with imaginary part set to the float and with
-- 0 real part
toImaginary : Float -> Complex
toImaginary x = Complex 0 x

----------------------------------------------------
-- THE FUNCTION TO REPRESENT THE JULIA SET
----------------------------------------------------

{-| Compute the number of iterations it takes for a 
complex number to diverge in the Julia Set. "maxIterations"
represent the ceiling for the maximum number of iterations
the function will take. "function" is the julia function
to be iterated. "constant" is the set constant in the
julia function. "complex" is the complex value of interest.
-}

julia : Int -> (Complex -> Complex -> Complex) -> Complex -> Complex -> Int
julia maxIterations function constant complex = 
  let juliaIter n f c z = 
    if | n > maxIterations -> maxIterations
       | cAbs z > 2        -> n
       | otherwise         -> juliaIter (n + 1) f c (f c z)
  in juliaIter 0 function constant complex
  
-----------------------------------------------------
-- FUNCTION TO DRAW A POINT FROM THE JULIA SET
-----------------------------------------------------

{-| Draw the Julia Set of a given complex function,
given complex constant at a given complex point.
Represents the points as a colored square. The color
shows the speed at which the point diverges from the
function.

    f c z = z `cMul` z `cAdd` c
    drawJulia f c z -- returns a colored square
-}
drawJulia : Int -> (Complex -> Complex -> Complex) -> Complex -> Complex -> Form
drawJulia maxIterations function constant complex =
  let juliaValue = julia maxIterations function constant complex
      ratio = (toFloat juliaValue) / (toFloat maxIterations)
      color = hsv (1 - (e ^ (e ^ ratio))) 0.7 1.0 
  in move ( complex.real      * (toFloat canvasWidth  / 4) 
          , complex.imaginary * (toFloat canvasHeight / 4))
          (filled color (square pointSize))
          
-------------------------------------------------------
-- FUNCTIONS TO DRAW THE GRAPH
-------------------------------------------------------

-- Helper function to draw all the points along the
-- "x" or "real" axis at a given  "y" or "imaginary" 
-- coordinate
drawRow : Float -> [Form]
drawRow y = 
  ( map (drawJulia maxIterations function constant)                . 
    map (flip Complex y)                    . 
    map (flip (/) ((toFloat xDensity) / 4)) .
    map (flip (-) ((toFloat xDensity) / 2)) .
    map toFloat
  ) [0..(xDensity)]
  
-- Function to draw all points in the Julia Set
drawAll : [Form]
drawAll =
  (concat                                   .
   map drawRow                              . 
   map (flip (/) ((toFloat yDensity) / 4))  .
   map (flip (-) ((toFloat yDensity) / 2))  .
   map toFloat
  ) [0..(yDensity)]

-- Render sets the width of the canvas and the 
-- height of the canvas as defined by "canvasWidth"
-- and "canvasHeight" respectively and then
-- calls "drawAll" which returns a list of all
-- the little colored squares that represent 
-- points in the complex plane. 
render : Element
render = 
  collage canvasWidth canvasHeight drawAll

--------------------------------------------------------
-- THE MAIN FUNCTION
--------------------------------------------------------
main = render