# Solving the last four corners of Rubik's cube

The problem of positioning and rotating the last four corners can be represented as a simple matrix problem, using complex numbers. I haven't tried, but an alternative to complex numbers would be to use integers and modulo. The complex numbers allows representation of rotation of a piece both around the cube axis normal to the plane containing the four corners, and along the axis through the centre and a given corner. Rotation is then simply multiplying by the copmlex number representing the desired angle.

There are four corners to fix, each can have any of the four corners in its place. Each piece may also be at one of three twisted positions.

As I write, I have found a set of eight transformations which preserve everything except the corners in question. They take sixteen moves per transformation. I am not trying to optimize to find shortcuts, just a solution from the point I have got to manually, using the strategy of finding these near-neutral transformations in long sequences.

The corner piece position is represented by position in the matrix, whereas the angle is represented by a complex number: 0 for correct position e^2i.pi/3 for one third rotation, and e^4i.pi/3 . This is all half remembered maths from school.

index corner is the one that doesn't change when T1 is applied (currently red-white-blue). Sequence is anti-clockwise when facing the four corners to be fixed.

Goal position (S0) is
0
0
0
0

Transformation matrix for the first of the eight transforms is
0 0 0 0
0 0 0 t
0 t 0 0
0 0 t 0

where t = e^2i.pi/3

The eight transformations are derived by cycling row-wise, and vertically mirroring each of the four. This means starting on a different corner of the cube when begging the sequence, and doing the sequence either left or right handed.

Start position, (currently!) is
1, (correct position, correct orientation)
4, correctly oriented (the second position contains the piece that should be in the fourth position)
2, one third turn anticlockwise needed to orient correctly
3, one third turn anticlockwise needed

Complex matrix description of this state:

0 0 0 0
0 0 t 0
0 0 0 t
0 t 0 0

There must be an infinite number of solutions. I just one a single solution. This solution is already very far from optimal. Any position can be solved in 26 moves total, according to an abstract I saw. My method is just an extension of a manual technique.

The equation to solve is:

Product (n=1..8) a(n).T(n).S(initial) = S0 

Most likely, it will only take two, maybe three Ts.

