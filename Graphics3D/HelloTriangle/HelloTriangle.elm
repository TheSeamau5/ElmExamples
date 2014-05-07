import MJS (..)
import Graphics.WebGL (..)

main = webgl (400, 400) scene

type Point = {point : V3}
toPoint vector = { point = vector }

triangle : V3 -> V3 -> V3 -> Triangle Point
triangle p1 p2 p3 = (toPoint p1, toPoint p2, toPoint p3)

basicTriangle : Triangle Point
basicTriangle = triangle (v3 0 0 0) (v3 0 1 0) (v3 1 0 0)

scene : [Model]
scene = [model vertexShader fragmentShader [basicTriangle] {}] 


vertexShader = [glShader|

attribute vec3 point;

void main (){
  gl_Position = vec4(point, 1.0);

}

|]

fragmentShader = [glShader| 

void main (){
  gl_FragColor = vec4(0.0,0.0,1.0,1.0);
}

|]