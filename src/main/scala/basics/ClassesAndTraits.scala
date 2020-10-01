package basics

object ClassesAndTraits {
  sealed trait Shape extends Located with Bounded with Movable with Areal

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  sealed trait Areal {
    def area: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = {
      Point(x + dx, y + dy)
    }
    override def area: Double = 0.0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def move(dx: Double, dy: Double): Circle = {
      this.copy(centerX = centerX + dx, centerY = centerY + dx)
    }
    override def area: Double = Math.PI * radius * radius
  }

  final case class Rectangle(minX: Double, minY: Double, width: Double, height: Double) extends Shape {
    override def x: Double = minX + width / 2
    override def y: Double = minY + height / 2
    override def maxX: Double = minX + width
    override def maxY: Double = minY + height
    override def move(dx: Double, dy: Double): Rectangle = {
      this.copy(minX = minX + dx, minY = minY + dy)
    }
    override def area: Double = width * height
  }

  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
    new Bounded {
      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
      override def minX: Double = objects.map(_.minX).min
      override def maxX: Double = objects.map(_.maxX).max
      override def minY: Double = objects.map(_.minY).min
      override def maxY: Double = objects.map(_.maxY).max
    }
  }

  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = ???
  }

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  // not a case class to allow inheritance
  class Polygon(vertices: List[Point]) extends Shape {
    // using simple arithmetic mean to find the centroid
    override def x =  vertices.map(_.x).sum / vertices.size
    override def y =  vertices.map(_.y).sum / vertices.size
    override def minX: Double = vertices.map(_.x).min
    override def maxX: Double = vertices.map(_.x).max
    override def minY: Double = vertices.map(_.y).min
    override def maxY: Double = vertices.map(_.y).max
    override def move(dx: Double, dy: Double): Polygon = {
      new Polygon(vertices.map(p => p.move(dx, dy)))
    }
    def area: Double = ???
  }

  final case class Triangle(vertices: List[Point]) extends Polygon(vertices) {
    require(vertices.size == 3)

    override def area: Double = (vertices(0).x * (vertices(1).y - vertices(2).y) +
      vertices(1).x * (vertices(2).y - vertices(0).y) +
      vertices(2).x * (vertices(0).y - vertices(1).y)).abs / 2
  }

  final case class Square(minX: Double, minY: Double, length: Double) extends Shape {
    override def x: Double = minX + length / 2
    override def y: Double = minY + length / 2
    override def maxX: Double = minX + length
    override def maxY: Double = minY + length
    override def move(dx: Double, dy: Double): Square = {
      this.copy(minX = minX + dx, minY = minY + dy)
    }
    override def area: Double = length * length
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  sealed trait Surfaced {
    def surfaceArea: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D with Surfaced

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Point3D = {
      Point3D(x + dx, y + dy, z + dz)
    }
    override def surfaceArea: Double = 0.0
  }

  final case class Cube(minX: Double, minY: Double, minZ: Double, length: Double) extends Shape3D {
    override def x: Double = minX + length / 2
    override def y: Double = minY + length / 2
    override def z: Double = minZ + length / 2
    override def maxX: Double = minX + length
    override def maxY: Double = minY + length
    override def maxZ: Double = minZ + length
    override def move(dx: Double, dy: Double, dz: Double): Cube = {
      this.copy(minX = minX + dx, minY = minY + dy, minZ = minZ + dz)
    }
    override def surfaceArea: Double = 6 * length * length
  }

  final case class Cuboid(minX: Double, minY: Double, minZ: Double, width: Double, height: Double, length: Double) extends Shape3D {
    override def x: Double = minX + width / 2
    override def y: Double = minY + height / 2
    override def z: Double = minZ + length / 2
    override def maxX: Double = minX + width
    override def maxY: Double = minY + height
    override def maxZ: Double = minZ + length
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = {
      this.copy(minX = minX + dx, minY = minY + dy, minZ = minZ + dy)
    }
    override def surfaceArea: Double = 2 * width * height + 2 * length * width + 2 * length * height
  }

  final case class Sphere(centerX: Double, centerY: Double, centerZ: Double, radius: Double) extends Shape3D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
    override def move(dx: Double, dy: Double, dz: Double): Sphere = {
      this.copy(centerX = centerX + dx, centerY = centerY + dx, centerZ = centerZ + dz)
    }
    override def surfaceArea: Double = 4 * Math.PI * radius * radius
  }

  final case class Cylinder(centerX: Double, centerY: Double, centerZ: Double, radius: Double, length: Double) extends Shape3D {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - length / 2
    override def maxZ: Double = z + length / 2
    override def move(dx: Double, dy: Double, dz: Double): Cylinder = {
      this.copy(centerX = centerX + dx, centerY = centerY + dx, centerZ = centerZ + dz)
    }
    override def surfaceArea: Double = 2 * Math.PI * radius * radius + 2 * Math.PI * length
  }
}
