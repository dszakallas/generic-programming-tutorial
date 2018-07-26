
case class Scene(shapes: Seq[Shape], meta: Meta)

type Coord = (Float, Float)

case class RGBA(r: Byte, g: Byte, b: Byte, a: Byte)

sealed trait Shape {
  val origin: Coord
  val z: Float
  val fill: RGBA
}
final case class Rectangle(origin: Coord, z: Float, fill: RGBA, dims: (Float, Float)) extends Shape
final case class Circle(origin: Coord, z: Float, fill: RGBA, radius: Float) extends Shape

final case class Meta(authorInfo: AuthorInfo, title: String)
final case class AuthorInfo(name: String, email: String)

import shapeless.labelled.FieldType
import shapeless.{HList, HNil, LabelledGeneric, Lazy, Witness, :: => :*:, _}

def absurd[T](void: CNil): T = throw new Error("ex falso quodlibet")

trait Serializer[T] {
  def serialize(t: T): String
}
object Serializer extends SerializerLowPrio {

  implicit def rgbaSerializer: Serializer[RGBA] = { case RGBA(r, g, b, a) =>
    "\"" + f"0x${(r << 24) + (g << 16) + (b << 8) + a}%08x" + "\""
  }

  implicit def coordSerializer: Serializer[Coord] = { case (x, y) =>
    s"[${x}, ${y}]"
  }

  implicit def floatSerializer: Serializer[Float] = _.toString

  private[this] def seqRecurse[T: Serializer](agg: String, xs: Seq[T]): String = xs match {
    case x :: xs => seqRecurse(agg + ", " + implicitly[Serializer[T]].serialize(x), xs)
    case Nil => agg + "]"
  }

  implicit def seqSerializer[T: Serializer]: Serializer[Seq[T]] = {
    case x :: xs => seqRecurse("[" + implicitly[Serializer[T]].serialize(x), xs)
    case Nil => "[]"
  }

  implicit val strSerializer: Serializer[String] = x => "\"" + x + "\""

}
trait SerializerLowPrio {
  implicit def genericSerializer[T, Repr](implicit
    t: LabelledGeneric.Aux[T, Repr],
    s: Lazy[Serializer[Repr]]): Serializer[T] = { x => "{" + s.value.serialize(t.to(x)) + "}" }

  implicit def productMiddleSerializer[K1 <: Symbol, K2 <: Symbol, H1, H2, T <: HList](implicit
    k: Witness.Aux[K1],
    sh: Serializer[H1],
    st: Lazy[Serializer[FieldType[K2, H2] :*: T]]
  ): Serializer[FieldType[K1, H1] :*: FieldType[K2, H2] :*: T] = x =>
    "\"" + k.value.name + "\": " + sh.serialize(x.head) + ", " + st.value.serialize(x.tail)

  implicit def productLastSerializer[K <: Symbol, H, T <: HList](implicit
    k: Witness.Aux[K],
    sh: Lazy[Serializer[H]]
  ): Serializer[FieldType[K, H] :*: HNil] = x =>
    "\"" + k.value.name  + "\": " + sh.value.serialize(x.head)

  implicit val hNilSerializer: Serializer[HNil] = x => ""

  implicit def coproductSerializer[K <: Symbol, H, T <: Coproduct](implicit
    k: Witness.Aux[K],
    sh: Serializer[H],
    st: Lazy[Serializer[T]]
  ): Serializer[FieldType[K, H] :+: T] = {
    case Inl(l) => "\"" + k.value.name + "\":" + sh.serialize(l)
    case Inr(r) => st.value.serialize(r)
  }

  def absurd[T](void: CNil): T = throw new Error("ex falso quodlibet")

  implicit val cNilFormat: Serializer[CNil] = absurd
}

import Serializer._

def serialize[T: Serializer](t: T) = implicitly[Serializer[T]].serialize(t)

serialize(5.0f)
serialize(RGBA(1, 2, 3, 4))
serialize((9.0f, 1.0f))
serialize("coffee")

serialize(Seq[Coord]())
serialize(Seq((9.0f, 1.0f)))
serialize(Seq((9.0f, 1.0f), (3.0f, 2.0f)))

serialize(Meta(AuthorInfo("David", "david@nomail.com"), "Tutorial"))

serialize(Rectangle((4.0f, 3.4f), 2.0f, RGBA(1, 2, 3, 4), (4.0f, 3.4f)))

AuthorInfo("someone", "someone@somewhere").productIterator.foreach(println)

val myList = 5 :: "five" :: HNil

println(myList.head)           // 5
println(myList.tail.head)      // five
// println(myList.tail.tail.head) Compile time error!


case class Silly(a: Int, b: String)

val ccg = Generic[Silly]
ccg.to(Silly(5, "five"))
ccg.from(5 :: "five" :: HNil)

val tg = Generic[(Int, String)]
tg.to((5, "five"))
tg.from(5 :: "five" :: HNil)

ccg.from(tg.to((5, "five")))

val lgen = LabelledGeneric[Silly]
lgen.to(Silly(5, "five"))


trait FieldNames[T] {
  val names: Seq[String]
}
object FieldNames {
  implicit def fieldNamesGeneric[T, Repr](implicit
    lgen: LabelledGeneric.Aux[T, Repr],
    f: Lazy[FieldNames[Repr]]
  ): FieldNames[T] = new FieldNames[T] {
    override val names = f.value.names
  }

  implicit def fieldNamesHCons[K <: Symbol, H, T <: HList](implicit
    f: Witness.Aux[K],
    rest: Lazy[FieldNames[T]]
  ): FieldNames[FieldType[K, H] :*: T] = new FieldNames[FieldType[K, H] :*: T] {
    override val names = f.value.name +: rest.value.names
  }

  implicit def fieldNamesHNil: FieldNames[HNil] = new FieldNames[HNil] {
    override val names: Seq[String] = Seq.empty
  }
}

implicitly[FieldNames[Silly]].names.foreach(println)

val union = Generic[Shape]

union.to(Rectangle((4.0f, 3.4f), 2.0f, RGBA(1, 2, 3, 4), (4.0f, 3.4f)))

union.to(Circle((4.0f, 3.4f), 2.0f, RGBA(1, 2, 3, 4), 6.0f))

serialize[Shape](Rectangle((4.0f, 3.4f), 2.0f, RGBA(1, 2, 3, 4), (4.0f, 3.4f)))

serialize(Scene(
  Seq(
    Rectangle((4f, 5f), 2.0f, RGBA(1, 2, 3, 4), (4.5f, 5.6f)),
    Circle((3f, 2f), 2.0f, RGBA(0, 0, 0, 0), 6)
  ),
  Meta(AuthorInfo("David", "david@nomail.com"), "Tutorial")))